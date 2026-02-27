(function () {
  "use strict";

  if (window.__posSoftNavBound) {
    return;
  }
  window.__posSoftNavBound = true;

  if (!window.fetch || !window.history || !window.URL) {
    return;
  }

  var pendingController = null;
  var navSeq = 0;

  /**
   * Executes the isModifiedClick function.
   * @param {*} event Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function isModifiedClick(event) {
    return event.metaKey || event.ctrlKey || event.shiftKey || event.altKey;
  }

  /**
   * Executes the shouldHandle function.
   * @param {*} anchor Input parameter used by this function.
  function shouldHandle(anchor, event) {
    if (!anchor || !anchor.getAttribute) {
      return false;
    }
    if (event.defaultPrevented || event.button !== 0 || isModifiedClick(event)) {
      return false;
    }
  /**
   * Executes the shouldHandle function.
   * @param {*} anchor Input parameter used by this function.
   * @param {*} event Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    if (anchor.hasAttribute("download") || anchor.getAttribute("target") === "_blank") {
  /**
   * Executes the shouldHandle function.
   * @param {*} anchor Input parameter used by this function.
   * @param {*} event Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
      return false;
    }
    if (anchor.getAttribute("rel") === "external" || anchor.hasAttribute("data-no-soft-nav")) {
      return false;
  /**
   * Executes the shouldHandle function.
   * @param {*} anchor Input parameter used by this function.
   * @param {*} event Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    }
    var href = anchor.getAttribute("href") || "";
    if (!href || href.indexOf("javascript:") === 0 || href.indexOf("mailto:") === 0 || href.indexOf("tel:") === 0) {
      return false;
    }
    return true;
  }

  /**
   * Executes the sameDocumentHashJump function.
   * @param {*} url Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function sameDocumentHashJump(url) {
    if (!url.hash) {
      return false;
    }
    return url.pathname === window.location.pathname && url.search === window.location.search;
  }

  /**
   * Executes the navigate function.
   * @param {*} rawUrl Input parameter used by this function.
   * @param {*} replaceState Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function navigate(rawUrl, replaceState) {
    var url;
    try {
      url = new URL(rawUrl, window.location.href);
    } catch (error) {
      return;
    }
  /**
   * Executes the navigate function.
   * @param {*} rawUrl Input parameter used by this function.
   * @param {*} replaceState Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the navigate function.
   * @param {*} rawUrl Input parameter used by this function.
   * @param {*} replaceState Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */

  /**
   * Executes the navigate function.
   * @param {*} rawUrl Input parameter used by this function.
   * @param {*} replaceState Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    if (url.origin !== window.location.origin) {
      window.location.href = url.href;
      return;
    }

    if (sameDocumentHashJump(url)) {
      window.location.hash = url.hash;
      return;
    }

    if (pendingController) {
      pendingController.abort();
    }

    var controller = new AbortController();
    pendingController = controller;
    var currentSeq = ++navSeq;

    document.documentElement.classList.add("soft-nav-loading");

    fetch(url.href, {
      method: "GET",
      credentials: "same-origin",
      signal: controller.signal,
      headers: {
        "X-Requested-With": "fetch",
        "X-Soft-Navigation": "true"
      }
    })
      .then(function (response) {
        if (!response.ok) {
          throw new Error("Navigation failed: " + response.status);
        }
        return response.text();
      })
      .then(function (html) {
        if (currentSeq !== navSeq) {
          return;
        }
        if (replaceState) {
          history.replaceState({ softNav: true }, "", url.href);
        } else {
          history.pushState({ softNav: true }, "", url.href);
        }
        document.open();
        document.write(html);
        document.close();
      })
      .catch(function (error) {
        if (controller.signal.aborted) {
          return;
        }
        window.location.href = url.href;
      })
      .finally(function () {
        if (currentSeq === navSeq) {
          document.documentElement.classList.remove("soft-nav-loading");
        }
      });
  }

  window.addEventListener(
    "click",
    function (event) {
      var target = event.target;
      if (!target || !target.closest) {
        return;
      }
      var anchor = target.closest("a[href]");
      if (!anchor) {
        return;
      }
      if (!anchor.closest("[data-soft-nav-root='true']")) {
        return;
      }
      if (!shouldHandle(anchor, event)) {
        return;
      }
      event.preventDefault();
      navigate(anchor.href, false);
    },
    true
  );

  window.addEventListener("popstate", function () {
    navigate(window.location.href, true);
  });
})();
