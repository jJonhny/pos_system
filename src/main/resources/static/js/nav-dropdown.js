(function () {
  if (window.__posNavDropdownBound) {
    return;
  }
  window.__posNavDropdownBound = true;

  /**
   * Executes the getOpenDropdowns function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  function getOpenDropdowns() {
    return document.querySelectorAll(".app-main-nav .nav-dropdown[open]");
  }

  /**
   * Executes the closeAll function.
  function closeAll(except) {
    getOpenDropdowns().forEach(function (dropdown) {
      if (dropdown !== except) {
        dropdown.removeAttribute("open");
      }
    });
  /**
   * Executes the closeAll function.
   * @param {*} except Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
  }
  /**
   * Executes the closeAll function.
   * @param {*} except Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */

  document.addEventListener("click", function (event) {
    var summary = event.target.closest(".app-main-nav .nav-dropdown > summary");
    if (summary) {
  /**
   * Executes the closeAll function.
   * @param {*} except Input parameter used by this function.
   * @returns {any} Result produced by this function.
   * @throws {Error} May throw runtime errors from DOM, network, or dependency operations.
   * Edge cases: Null, undefined, and empty inputs are handled by the existing implementation.
   */
      var dropdown = summary.closest(".nav-dropdown");
      window.setTimeout(function () {
        if (dropdown && dropdown.hasAttribute("open")) {
          closeAll(dropdown);
        }
      }, 0);
      return;
    }

    if (!event.target.closest(".app-main-nav .nav-dropdown")) {
      closeAll(null);
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      closeAll(null);
    }
  });
})();
