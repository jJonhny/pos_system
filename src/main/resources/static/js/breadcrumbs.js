(function () {
  "use strict";

  if (window.__posBreadcrumbsBound) {
    return;
  }
  window.__posBreadcrumbsBound = true;

  var SVG_NS = "http://www.w3.org/2000/svg";

  var SEGMENT_META = {
    "pos": { label: "POS", icon: "pos" },
    "sales": { label: "Sales", icon: "sales" },
    "commodity": { label: "Commodity", icon: "commodity" },
    "marketing": { label: "Marketing", icon: "marketing" },
    "currencies": { label: "Financial", icon: "financial" },
    "pos-setting": { label: "POS Setting", icon: "settings" },
    "users": { label: "User", icon: "user" },
    "user": { label: "User", icon: "user" },
    "account": { label: "Account", icon: "user" },
    "analytics": { label: "Analytics", icon: "analytics" },
    "reports": { label: "Reports", icon: "reports" },
    "admin": { label: "Admin", icon: "admin" },
    "audit": { label: "Audit", icon: "audit" },
    "audit-events": { label: "Audit Events", icon: "audit" },
    "products": { label: "Products", icon: "products" },
    "categories": { label: "Categories", icon: "categories" },
    "suppliers": { label: "Suppliers", icon: "suppliers" },
    "purchases": { label: "Purchases", icon: "purchases" },
    "inventory": { label: "Inventory", icon: "inventory" },
    "inventory-ledger": { label: "Inventory Ledger", icon: "ledger" },
    "receiving": { label: "Receiving", icon: "receiving" },
    "password": { label: "Password", icon: "lock" },
    "login": { label: "Login", icon: "lock" },
    "return": { label: "Return", icon: "sales" },
    "receipt": { label: "Receipt", icon: "reports" }
  };

  var ICON_PATHS = {
    "home": ["M3 10.5 12 3l9 7.5", "M5 10v10h14V10", "M9 20v-6h6v6"],
    "pos": ["M6 6h12v4H6z", "M6 10h12v8a2 2 0 0 1-2 2H8a2 2 0 0 1-2-2v-8z", "M9 14h6"],
    "sales": ["M7 3h10v18l-2-1-2 1-2-1-2 1-2-1-2 1V3z", "M9 7h6", "M9 11h6", "M9 15h4"],
    "commodity": ["M21 8l-9 5-9-5 9-5 9 5z", "M3 8v8l9 5 9-5V8"],
    "marketing": ["M3 7h18", "M5 7v11a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V7", "M9 11h6", "M9 15h6"],
    "financial": ["M12 3a9 9 0 1 0 0 18a9 9 0 0 0 0-18z", "M8 10h8", "M8 14h8", "M12 7v10"],
    "settings": ["M12 8a4 4 0 1 0 0 8a4 4 0 0 0 0-8z", "M3 12h2m14 0h2M12 3v2m0 14v2M5.6 5.6l1.4 1.4m10 10l1.4 1.4M18.4 5.6L17 7m-10 10l-1.4 1.4"],
    "user": ["M12 12a5 5 0 1 0 0-10a5 5 0 0 0 0 10z", "M3 21a9 9 0 0 1 18 0"],
    "analytics": ["M3 3v18h18", "M7 14l3-3 4 4 5-6"],
    "reports": ["M4 4h16v16H4z", "M8 8h8", "M8 12h8", "M8 16h6"],
    "admin": ["M12 3l7 4v10l-7 4-7-4V7l7-4z", "M9 12h6", "M12 9v6"],
    "audit": ["M5 4h14v16H5z", "M8 8h8", "M8 12h8", "M8 16h5"],
    "products": ["M3 7l9-4 9 4-9 4-9-4z", "M3 7v10l9 4 9-4V7"],
    "categories": ["M3 3h8v8H3z", "M13 3h8v8h-8z", "M3 13h8v8H3z", "M13 13h8v8h-8z"],
    "suppliers": ["M3 20h18", "M5 20V9l7-4 7 4v11", "M9 13h6"],
    "purchases": ["M6 7h12l-1 12H7L6 7z", "M8.5 7a3.5 3.5 0 0 1 7 0"],
    "inventory": ["M4 6h16", "M4 12h16", "M4 18h16", "M8 6v12"],
    "ledger": ["M5 4h14v16H5z", "M9 8h6", "M9 12h6", "M9 16h6"],
    "receiving": ["M3 7h18", "M5 7v11a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V7", "M9 11h6"],
    "lock": ["M7 11h10v10H7z", "M9 11V8a3 3 0 0 1 6 0v3"],
    "chevron": ["M9 6l6 6-6 6"],
    "default": ["M3 7h6l2 2h10v10H3V7z"]
  };

  var SKIP_SEGMENTS = {
    "list": true,
    "index": true
  };

  /**
   * Executes the ensureBreadcrumbStyles function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function ensureBreadcrumbStyles() {
    if (document.getElementById("app-breadcrumb-styles")) {
      return;
    }

    var style = document.createElement("style");
  /**
   * Executes the ensureBreadcrumbStyles function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the ensureBreadcrumbStyles function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    style.id = "app-breadcrumb-styles";
    style.textContent = [
      ".app-breadcrumb-wrap{margin:0 0 1rem;}",
      ".app-breadcrumb-shell{display:block;}",
      ".app-breadcrumb-list{display:flex;align-items:center;flex-wrap:wrap;gap:.35rem;list-style:none;margin:0;padding:0;}",
      ".app-breadcrumb-item{display:inline-flex;align-items:center;gap:.35rem;min-width:0;}",
      ".app-breadcrumb-link,.app-breadcrumb-current{display:inline-flex;align-items:center;gap:.35rem;padding:.3rem .55rem;border-radius:.6rem;font-size:.78rem;line-height:1;color:#475569;text-decoration:none;border:1px solid transparent;}",
      ".app-breadcrumb-link{background:#fff;border-color:#e2e8f0;}",
      ".app-breadcrumb-link:hover{background:#f8fafc;border-color:#cbd5e1;color:#334155;}",
      ".app-breadcrumb-current{background:#f1f5f9;color:#0f172a;font-weight:700;}",
      ".app-breadcrumb-icon{color:#64748b;flex:0 0 auto;}",
      ".app-breadcrumb-sep{color:#94a3b8;flex:0 0 auto;}",
      ".app-breadcrumb-label{display:inline-block;white-space:nowrap;}"
    ].join("");
    document.head.appendChild(style);
  }

  /**
   * Executes the decodeSegment function.
   * @param {*} value Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function decodeSegment(value) {
    try {
      return decodeURIComponent(value || "");
    } catch (error) {
      return value || "";
    }
  }

  /**
   * Executes the isLikelyId function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function isLikelyId(segment) {
    return /^[0-9]+$/.test(segment)
      || /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(segment)
      || /^[0-9a-f]{16,}$/i.test(segment);
  }

  /**
  /**
   * Executes the titleCase function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function titleCase(segment) {
    return decodeSegment(segment)
      .replace(/[-_]+/g, " ")
      .replace(/\s+/g, " ")
      .trim()
      .replace(/\b\w/g, function (char) { return char.toUpperCase(); });
  }
  /**
   * Executes the titleCase function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the titleCase function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */

  /**
   * Executes the findTopNavLabel function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
  /**
   * Executes the findTopNavLabel function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function findTopNavLabel(segment) {
    if (!segment) {
      return "";
    }
    var anchors = document.querySelectorAll(".app-main-nav a[href]");
    var expectedHref = "/" + segment;
    for (var i = 0; i < anchors.length; i += 1) {
  /**
   * Executes the findTopNavLabel function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
      if (anchors[i].getAttribute("href") === expectedHref) {
        var span = anchors[i].querySelector("span");
  /**
   * Executes the findTopNavLabel function.
   * @param {*} segment Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
        var text = span ? span.textContent : anchors[i].textContent;
        if (text && text.trim()) {
          return text.trim();
        }
      }
    }
    return "";
  }

  /**
   * Executes the createIcon function.
   * @param {*} iconKey Input parameter used by this function.
   * @param {*} className Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function createIcon(iconKey, className) {
    var paths = ICON_PATHS[iconKey] || ICON_PATHS.default;
    var isSeparator = className === "app-breadcrumb-sep";
    var iconSize = isSeparator ? 12 : 15;
    var svg = document.createElementNS(SVG_NS, "svg");
    svg.setAttribute("viewBox", "0 0 24 24");
    svg.setAttribute("fill", "none");
    svg.setAttribute("stroke", "currentColor");
  /**
   * Executes the createIcon function.
   * @param {*} iconKey Input parameter used by this function.
   * @param {*} className Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the createIcon function.
   * @param {*} iconKey Input parameter used by this function.
   * @param {*} className Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    svg.setAttribute("stroke-width", "2");
    svg.setAttribute("stroke-linecap", "round");
    svg.setAttribute("stroke-linejoin", "round");
    svg.setAttribute("aria-hidden", "true");
    svg.setAttribute("focusable", "false");
    svg.setAttribute("class", className || "app-breadcrumb-icon");
    // Guard against page-level SVG rules that can inflate icons to viewport size.
    svg.setAttribute("width", String(iconSize));
    svg.setAttribute("height", String(iconSize));
    svg.style.width = iconSize + "px";
    svg.style.height = iconSize + "px";
    svg.style.minWidth = iconSize + "px";
    svg.style.minHeight = iconSize + "px";
    svg.style.maxWidth = iconSize + "px";
    svg.style.maxHeight = iconSize + "px";
    svg.style.flex = "0 0 " + iconSize + "px";
    for (var i = 0; i < paths.length; i += 1) {
      var path = document.createElementNS(SVG_NS, "path");
      path.setAttribute("d", paths[i]);
      svg.appendChild(path);
    }
    return svg;
  }

  /**
   * Executes the getHomeLabel function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function getHomeLabel() {
    var lang = (document.documentElement.getAttribute("lang") || "en").toLowerCase();
    return lang.indexOf("zh") === 0 ? "\u9996\u9875" : "Home";
  }

  /**
  function resolveMeta(segment, index) {
    var key = (segment || "").toLowerCase();
    var meta = SEGMENT_META[key];
    if (!meta) {
      return {
        label: titleCase(segment),
  /**
   * Executes the resolveMeta function.
   * @param {*} segment Input parameter used by this function.
   * @param {*} index Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
        icon: index === 0 ? "default" : "default"
      };
  /**
   * Executes the resolveMeta function.
   * @param {*} segment Input parameter used by this function.
   * @param {*} index Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    }
    if (index === 0) {
      var navLabel = findTopNavLabel(segment);
      if (navLabel) {
        return { label: navLabel, icon: meta.icon };
  /**
   * Executes the resolveMeta function.
   * @param {*} segment Input parameter used by this function.
   * @param {*} index Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
      }
    }
    return meta;
  }

  /**
   * Executes the buildCrumbs function.
   * @param {*} pathname Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function buildCrumbs(pathname) {
    var normalized = (pathname || "/").replace(/\/+/g, "/").replace(/\/$/, "");
    if (!normalized) {
      normalized = "/";
    }

    var segments = normalized === "/" ? [] : normalized.replace(/^\//, "").split("/");
  /**
   * Executes the buildCrumbs function.
   * @param {*} pathname Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the buildCrumbs function.
   * @param {*} pathname Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    var rawCrumbs = [];
    var hrefPath = "";
    for (var i = 0; i < segments.length; i += 1) {
      var segment = segments[i];
      hrefPath += "/" + segment;
      if (!segment || SKIP_SEGMENTS[segment] || isLikelyId(segment)) {
        continue;
      }
      var meta = resolveMeta(segment, i);
      rawCrumbs.push({
        href: hrefPath,
        label: meta.label,
        icon: meta.icon
      });
    }

    var crumbs = [{
      href: rawCrumbs.length ? "/" : null,
      label: getHomeLabel(),
      icon: "home",
      current: rawCrumbs.length === 0
    }];

    for (var j = 0; j < rawCrumbs.length; j += 1) {
      crumbs.push({
        href: j === rawCrumbs.length - 1 ? null : rawCrumbs[j].href,
        label: rawCrumbs[j].label,
        icon: rawCrumbs[j].icon,
        current: j === rawCrumbs.length - 1
      });
    }

    return crumbs;
  }

  /**
   * Executes the createCrumbNode function.
   * @param {*} crumb Input parameter used by this function.
   * @param {*} isLast Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function createCrumbNode(crumb, isLast) {
    var item = document.createElement("li");
    item.className = "app-breadcrumb-item";

    var node;
    if (crumb.href) {
      node = document.createElement("a");
      node.className = "app-breadcrumb-link";
  /**
   * Executes the createCrumbNode function.
   * @param {*} crumb Input parameter used by this function.
   * @param {*} isLast Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the createCrumbNode function.
   * @param {*} crumb Input parameter used by this function.
   * @param {*} isLast Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
      node.href = crumb.href;
    } else {
      node = document.createElement("span");
      node.className = "app-breadcrumb-current";
      node.setAttribute("aria-current", "page");
    }

    node.appendChild(createIcon(crumb.icon, "app-breadcrumb-icon"));

    var label = document.createElement("span");
    label.className = "app-breadcrumb-label";
    label.textContent = crumb.label;
    node.appendChild(label);
    item.appendChild(node);

    if (!isLast) {
      var sep = createIcon("chevron", "app-breadcrumb-sep");
      item.appendChild(sep);
    }

    return item;
  }

  /**
   * Executes the renderBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function renderBreadcrumbs() {
    if (document.body && document.body.getAttribute("data-disable-breadcrumbs") === "true") {
      return true;
    }

    ensureBreadcrumbStyles();
  /**
   * Executes the renderBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the renderBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */

    var main = document.querySelector("main");
    if (!main) {
      return false;
    }

    if (main.querySelector("[data-app-breadcrumb='true']")) {
      return true;
    }

    var wrap = document.createElement("div");
    wrap.className = "app-breadcrumb-wrap";
    wrap.setAttribute("data-app-breadcrumb", "true");

    var nav = document.createElement("nav");
    nav.className = "app-breadcrumb-shell";
    nav.setAttribute("aria-label", "Breadcrumb");

    var list = document.createElement("ol");
    list.className = "app-breadcrumb-list";

    var crumbs = buildCrumbs(window.location.pathname || "/");
    for (var i = 0; i < crumbs.length; i += 1) {
      list.appendChild(createCrumbNode(crumbs[i], i === crumbs.length - 1));
    }

    nav.appendChild(list);
    wrap.appendChild(nav);
    main.insertBefore(wrap, main.firstChild);
    return true;
  }

  /**
   * Executes the bootstrapBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function bootstrapBreadcrumbs() {
    if (renderBreadcrumbs()) {
      return;
    }

    var observer = null;
  /**
   * Executes the bootstrapBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  /**
   * Executes the bootstrapBreadcrumbs function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
    var pollTimer = null;
    var attempts = 0;
    var maxAttempts = 120;

    /**
     * Executes the tryRender function.
     * @returns {any} Result produced by this function.
     * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
     * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
     */
    function tryRender() {
      attempts += 1;
      var done = renderBreadcrumbs();
      if (done || attempts >= maxAttempts) {
        if (observer) {
          observer.disconnect();
    /**
     * Executes the tryRender function.
     * @returns {any} Result produced by this function.
     * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
     * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
     */
    /**
     * Executes the tryRender function.
     * @returns {any} Result produced by this function.
     * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
     * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
     */
        }
        if (pollTimer) {
          clearInterval(pollTimer);
        }
      }
    }

    if (typeof MutationObserver !== "undefined") {
      observer = new MutationObserver(tryRender);
      observer.observe(document.documentElement, { childList: true, subtree: true });
    }

    pollTimer = setInterval(tryRender, 50);

    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", tryRender, { once: true });
    } else {
      setTimeout(tryRender, 0);
    }

    window.addEventListener("pageshow", tryRender, { once: true });
  }

  bootstrapBreadcrumbs();
})();
