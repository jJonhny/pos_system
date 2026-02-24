(function(global) {
  'use strict';

  // Configuration defaults that can be overridden via data attributes or the init() call.
  const DEFAULTS = {
    selectSelector: '[data-lang-switcher-select="true"]',
    formSelector: '[data-lang-switcher-form="true"]',
    preserveInputSelector: 'input[data-lang-preserve="true"]',
    urlParam: 'lang',
    method: 'auto',                 // 'form', 'redirect', or 'auto' (if inside a form, use form)
    usePushState: false,             // whether to use history.pushState instead of redirect
    preserveQueryParams: true,
    logLevel: 'error',               // 'debug', 'info', 'warn', 'error', 'none'
    ariaLive: 'polite',              // ARIA live region to announce language change
  };

  // Simple logger with levels
  const LOG_LEVELS = { debug: 0, info: 1, warn: 2, error: 3, none: 4 };
  class Logger {
    constructor(level = 'error') {
      this.level = LOG_LEVELS[level] ?? LOG_LEVELS.error;
    }
    debug(...args) { if (this.level <= LOG_LEVELS.debug) console.debug('[LangSwitcher]', ...args); }
    info(...args)  { if (this.level <= LOG_LEVELS.info)  console.info('[LangSwitcher]', ...args); }
    warn(...args)  { if (this.level <= LOG_LEVELS.warn)  console.warn('[LangSwitcher]', ...args); }
    error(...args) { if (this.level <= LOG_LEVELS.error) console.error('[LangSwitcher]', ...args); }
  }

  class LangSwitcher {
    constructor(options = {}) {
      this.options = { ...DEFAULTS, ...options };
      this.logger = new Logger(this.options.logLevel);
      this.boundSelects = new WeakSet();   // track already initialised selects
      this.formInputsCache = new WeakMap(); // cache for preserved inputs per form
      this.changeHandler = this.handleChange.bind(this);
      this.init();
    }

    // Main initialisation – sets up event delegation on document
    init() {
      try {
        // Use event delegation to catch changes on any matching select,
        // even if added dynamically later.
        document.addEventListener('change', this.changeHandler);
        // Also scan existing elements to mark them as "bound" (optional)
        this.scanExisting();
        this.logger.debug('LangSwitcher initialised');
      } catch (err) {
        this.logger.error('Initialisation failed:', err);
      }
    }

    // Scan the DOM for existing switchers and prepare their forms if needed
    scanExisting() {
      const selects = document.querySelectorAll(this.options.selectSelector);
      selects.forEach(select => {
        if (!this.boundSelects.has(select)) {
          this.prepareForm(select);
          this.boundSelects.add(select);
        }
      });
    }

    // For a given select, find its associated form and add preserved query parameters
    prepareForm(select) {
      if (!this.options.preserveQueryParams) return;

      const form = select.closest(this.options.formSelector);
      if (!form) return;

      // Remove previously added preserved inputs for this form (if any)
      const oldInputs = form.querySelectorAll(this.options.preserveInputSelector);
      oldInputs.forEach(input => input.remove());

      // Add current query parameters (except the language param) as hidden inputs
      try {
        const url = new URL(window.location.href);
        for (const [key, value] of url.searchParams.entries()) {
          if (key === this.options.urlParam) continue;
          const hidden = document.createElement('input');
          hidden.type = 'hidden';
          hidden.name = key;
          hidden.value = value;
          hidden.setAttribute('data-lang-preserve', 'true');
          form.appendChild(hidden);
        }
        this.logger.debug('Preserved query params added to form', form);
      } catch (err) {
        this.logger.error('Failed to preserve query params:', err);
      }
    }

    // Event handler for change events
    handleChange(event) {
      const select = event.target;
      // Check if this is a language switcher select
      if (!select.matches(this.options.selectSelector)) return;

      // If we haven't prepared its form yet (e.g., it was added dynamically), do so now
      if (!this.boundSelects.has(select)) {
        this.prepareForm(select);
        this.boundSelects.add(select);
      }

      try {
        this.execute(select);
      } catch (err) {
        this.logger.error('Error during language switch:', err);
      }
    }

    // Execute the language switch for a given select element
    execute(select) {
      const form = select.closest(this.options.formSelector);
      const method = this.resolveMethod(select, form);
      const lang = select.value.trim();

      if (!lang) {
        this.logger.warn('No language value selected');
        return;
      }

      // Announce the change for screen readers (optional)
      this.announce(lang);

      if (method === 'form' && form) {
        // Submit the surrounding form
        form.submit();
        this.logger.debug('Form submitted', form);
      } else if (method === 'redirect') {
        // Redirect with the language parameter
        this.redirect(lang);
      } else {
        this.logger.warn('No valid method (form or redirect) available');
      }
    }

    // Determine which method to use based on options and presence of a form
    resolveMethod(select, form) {
      const methodAttr = select.dataset.langMethod || this.options.method;
      if (methodAttr === 'form') return 'form';
      if (methodAttr === 'redirect') return 'redirect';
      // 'auto' – use form if available, else redirect
      return form ? 'form' : 'redirect';
    }

    // Redirect (or pushState) to the new URL with updated language parameter
    redirect(lang) {
      try {
        const url = new URL(window.location.href);
        url.searchParams.set(this.options.urlParam, lang);

        if (this.options.usePushState) {
          history.pushState(null, '', url.toString());
          // Optionally dispatch a custom event so other parts of the app can react
          window.dispatchEvent(new CustomEvent('langswitcher:changed', { detail: { lang } }));
          this.logger.debug('History state updated', url.toString());
        } else {
          window.location.assign(url.toString());
          this.logger.debug('Redirecting to', url.toString());
        }
      } catch (err) {
        this.logger.error('Redirect failed:', err);
      }
    }

    // Simple ARIA live announcement (if there is a live region)
    announce(lang) {
      const liveRegion = document.querySelector('[aria-live="polite"], [aria-live="assertive"]');
      if (liveRegion && this.options.ariaLive !== 'none') {
        liveRegion.textContent = `Language changed to ${lang}`;
      }
    }

    // Clean up event listeners (useful for single-page apps)
    destroy() {
      document.removeEventListener('change', this.changeHandler);
      this.logger.debug('LangSwitcher destroyed');
    }
  }

  // Expose the constructor globally
  global.LangSwitcher = LangSwitcher;

  // Auto‑initialise on DOMContentLoaded if there is a data‑attribute on the document
  // This provides the same "drop‑in" behaviour as the original script.
  function autoInit() {
    if (document.documentElement.hasAttribute('data-lang-switcher-auto')) {
      new LangSwitcher();
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', autoInit, { once: true });
  } else {
    autoInit();
  }

})(typeof window !== 'undefined' ? window : this);