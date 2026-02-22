(() => {
  const bind = () => {
    const select = document.querySelector("[data-lang-switcher-select='true']");
    if (!select || select.dataset.langBound === "true") {
      return;
    }

    const form = select.closest("[data-lang-switcher-form='true']");
    if (form) {
      const url = new URL(window.location.href);
      form.querySelectorAll("input[data-lang-preserve='true']").forEach((node) => node.remove());
      for (const [key, value] of url.searchParams.entries()) {
        if (key === "lang") continue;
        const hidden = document.createElement("input");
        hidden.type = "hidden";
        hidden.name = key;
        hidden.value = value;
        hidden.dataset.langPreserve = "true";
        form.appendChild(hidden);
      }
    }

    select.dataset.langBound = "true";
    select.addEventListener("change", () => {
      if (form) {
        form.submit();
        return;
      }
      const next = select.value;
      if (!next) return;
      const url = new URL(window.location.href);
      url.searchParams.set("lang", next);
      window.location.assign(url.toString());
    });
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bind, { once: true });
  } else {
    bind();
  }
})();
