document.addEventListener('DOMContentLoaded', function () {
  var content = document.getElementById('content');
  if (!content) return;

  var root = document.documentElement;
  var body = document.body;
  var currentTheme = null;
  var themeToggleBtn = null;

  function cleanupLegacyBearblog() {
    Array.prototype.slice.call(document.querySelectorAll('#hb-topbar')).forEach(function (el) {
      el.parentNode.removeChild(el);
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-fold-toggle')).forEach(function (el) {
      el.parentNode.removeChild(el);
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-collapsible-heading, .hb-heading-collapsed')).forEach(function (el) {
      el.classList.remove('hb-collapsible-heading');
      el.classList.remove('hb-heading-collapsed');
    });

    Array.prototype.slice.call(content.querySelectorAll('.hb-section-body')).forEach(function (wrapper) {
      wrapper.classList.remove('hb-collapsed');
      wrapper.removeAttribute('data-collapsed');
      while (wrapper.firstChild) {
        wrapper.parentNode.insertBefore(wrapper.firstChild, wrapper);
      }
      wrapper.parentNode.removeChild(wrapper);
    });

    var legacyHeader = content.querySelector(':scope > header');
    if (legacyHeader) legacyHeader.parentNode.removeChild(legacyHeader);
  }

  function applyTheme(theme) {
    if (theme !== 'light' && theme !== 'dark') return;
    currentTheme = theme;
    root.setAttribute('data-theme', theme);
    root.classList.toggle('light', theme === 'light');
    root.classList.toggle('dark', theme === 'dark');

    try {
      window.localStorage.setItem('hj-theme', theme);
    } catch (e) {}

    if (themeToggleBtn) {
      themeToggleBtn.textContent = theme === 'dark' ? '日间模式 (Light Mode)' : '夜间模式 (Night Mode)';
      themeToggleBtn.setAttribute('aria-label', theme === 'dark' ? 'Switch to light mode' : 'Switch to night mode');
    }
  }

  function initTheme() {
    var stored = null;
    try {
      stored = window.localStorage.getItem('hj-theme');
    } catch (e) {}

    var initial = stored;
    if (initial !== 'light' && initial !== 'dark') {
      initial = root.getAttribute('data-theme');
    }
    if (initial !== 'light' && initial !== 'dark') {
      initial = 'light';
    }
    applyTheme(initial);
  }

  function cleanText(text) {
    return (text || '').replace(/\s+/g, ' ').trim();
  }

  function splitTitle(rawTitle) {
    var title = cleanText(rawTitle || document.title || 'henri-journal');
    var match = title.match(/^(.*?)(\d{4}-\d{2})$/);
    if (!match) return { main: title, date: '' };
    return {
      main: cleanText(match[1]),
      date: match[2]
    };
  }

  function movePostambleMeta() {
    var postamble = document.getElementById('postamble');
    var meta = [];
    if (!postamble) return meta;

    ['author', 'date'].forEach(function (className) {
      var el = postamble.querySelector('.' + className);
      if (!el) return;
      var text = cleanText(el.textContent.replace(/^(Author|Created):\s*/i, ''));
      if (text) meta.push(text);
    });

    postamble.parentNode.removeChild(postamble);
    return meta;
  }

  function buildHeader() {
    var titleHeading = content.querySelector('h1.title');
    var titleParts = splitTitle(titleHeading ? titleHeading.textContent : document.title);
    var metaValues = movePostambleMeta();

    var header = document.createElement('header');
    header.className = 'hj-site-header';

    var inner = document.createElement('div');
    inner.className = 'hj-site-header-inner';
    header.appendChild(inner);

    var titleBlock = document.createElement('div');
    titleBlock.className = 'hj-title-block';
    inner.appendChild(titleBlock);

    var kicker = document.createElement('p');
    kicker.className = 'hj-kicker';
    kicker.textContent = 'Commonplace Book';
    titleBlock.appendChild(kicker);

    var titleRow = document.createElement('div');
    titleRow.className = 'hj-title-row';
    titleBlock.appendChild(titleRow);

    var h1 = document.createElement('h1');
    h1.className = 'hj-page-title';
    h1.textContent = titleParts.main || 'henri-journal';
    titleRow.appendChild(h1);

    if (titleParts.date) {
      var date = document.createElement('span');
      date.className = 'hj-title-date';
      date.textContent = titleParts.date;
      titleRow.appendChild(date);
    }

    if (metaValues.length) {
      var meta = document.createElement('div');
      meta.className = 'hj-meta';
      metaValues.forEach(function (value) {
        var item = document.createElement('span');
        item.className = 'hj-meta-item';
        item.textContent = value;
        meta.appendChild(item);
      });
      titleBlock.appendChild(meta);
    }

    var actions = document.createElement('div');
    actions.className = 'hj-header-actions';
    inner.appendChild(actions);

    var themeBtn = document.createElement('button');
    themeBtn.id = 'hj-theme-toggle';
    themeBtn.className = 'hj-button';
    themeBtn.type = 'button';
    themeBtn.addEventListener('click', function () {
      applyTheme(currentTheme === 'dark' ? 'light' : 'dark');
    });
    themeToggleBtn = themeBtn;
    actions.appendChild(themeBtn);

    body.insertBefore(header, content);
    applyTheme(currentTheme || 'light');
    cleanupLegacyBearblog();
  }

  function parseHeadingDate(heading) {
    var text = cleanText(heading.textContent);
    var match = text.match(/\b(\d{4})-(\d{2})-(\d{2})\b/);
    if (!match) return null;
    return {
      key: match[1] + '-' + match[2] + '-' + match[3],
      year: parseInt(match[1], 10),
      month: parseInt(match[2], 10),
      day: parseInt(match[3], 10),
      heading: heading
    };
  }

  function collectJournalDays() {
    var candidates = Array.prototype.slice.call(content.querySelectorAll('h2, h3, h4'));
    var seen = {};
    var days = [];

    candidates.forEach(function (heading) {
      if (heading.closest('#table-of-contents')) return;
      var entry = parseHeadingDate(heading);
      if (!entry || seen[entry.key]) return;
      seen[entry.key] = true;
      if (!heading.id) heading.id = 'hj-day-' + entry.key;
      days.push(entry);
    });

    days.sort(function (a, b) {
      return a.key.localeCompare(b.key);
    });

    return days;
  }

  function monthName(year, month) {
    var date = new Date(year, month - 1, 1);
    return date.toLocaleDateString(undefined, { year: 'numeric', month: 'long' });
  }

  function buildCalendarGrid(days) {
    var wrap = document.createElement('div');
    wrap.className = 'hj-calendar';

    if (!days.length) {
      var empty = document.createElement('p');
      empty.className = 'hj-calendar-empty';
      empty.textContent = 'No dated journal entries found in this export.';
      wrap.appendChild(empty);
      return wrap;
    }

    var first = days[0];
    var activeByDay = {};
    days.forEach(function (entry) {
      activeByDay[entry.day] = entry;
    });

    var heading = document.createElement('h3');
    heading.className = 'hj-calendar-month';
    heading.textContent = monthName(first.year, first.month);
    wrap.appendChild(heading);

    var weekdays = document.createElement('div');
    weekdays.className = 'hj-weekdays';
    ['S', 'M', 'T', 'W', 'T', 'F', 'S'].forEach(function (name) {
      var el = document.createElement('div');
      el.textContent = name;
      weekdays.appendChild(el);
    });
    wrap.appendChild(weekdays);

    var grid = document.createElement('div');
    grid.className = 'hj-calendar-grid';
    wrap.appendChild(grid);

    var firstDate = new Date(first.year, first.month - 1, 1);
    var daysInMonth = new Date(first.year, first.month, 0).getDate();
    var startOffset = firstDate.getDay();
    var today = new Date();
    var todayKey = today.getFullYear() + '-' + String(today.getMonth() + 1).padStart(2, '0') + '-' + String(today.getDate()).padStart(2, '0');

    for (var i = 0; i < startOffset; i++) {
      var spacer = document.createElement('div');
      spacer.className = 'hj-day hj-empty';
      grid.appendChild(spacer);
    }

    for (var day = 1; day <= daysInMonth; day++) {
      var entry = activeByDay[day];
      var cell = document.createElement(entry ? 'button' : 'div');
      cell.className = 'hj-day' + (entry ? ' hj-active' : '');
      cell.textContent = String(day);

      var key = first.year + '-' + String(first.month).padStart(2, '0') + '-' + String(day).padStart(2, '0');
      if (key === todayKey) cell.classList.add('hj-today');

      if (entry) {
        cell.type = 'button';
        cell.setAttribute('aria-label', 'Jump to ' + entry.key);
        cell.addEventListener('click', function (targetEntry) {
          return function () {
            closeModal();
            targetEntry.heading.scrollIntoView({ behavior: 'smooth', block: 'start' });
            window.setTimeout(function () {
              targetEntry.heading.focus && targetEntry.heading.focus();
            }, 250);
          };
        }(entry));
      }

      grid.appendChild(cell);
    }

    var legend = document.createElement('div');
    legend.className = 'hj-calendar-legend';
    legend.innerHTML = '<span>Less active</span><span class="hj-legend-scale"><span class="hj-legend-box"></span><span class="hj-legend-box"></span><span class="hj-legend-box"></span></span><span>More active</span>';
    wrap.appendChild(legend);

    return wrap;
  }

  var modalBackdrop = null;
  var previouslyFocused = null;

  function openModal() {
    if (!modalBackdrop) return;
    previouslyFocused = document.activeElement;
    modalBackdrop.classList.add('hj-open');
    body.classList.add('hj-modal-open');
    modalBackdrop.setAttribute('aria-hidden', 'false');
    var close = modalBackdrop.querySelector('#hj-activity-close');
    if (close) close.focus();
  }

  function closeModal() {
    if (!modalBackdrop) return;
    modalBackdrop.classList.remove('hj-open');
    body.classList.remove('hj-modal-open');
    modalBackdrop.setAttribute('aria-hidden', 'true');
    if (previouslyFocused && previouslyFocused.focus) previouslyFocused.focus();
  }

  function buildActivity(days) {
    var launcher = document.createElement('div');
    launcher.className = 'hj-activity-launcher';
    var openBtn = document.createElement('button');
    openBtn.id = 'hj-activity-open';
    openBtn.className = 'hj-button';
    openBtn.type = 'button';
    openBtn.textContent = 'Activity Overview';
    launcher.appendChild(openBtn);
    body.appendChild(launcher);

    modalBackdrop = document.createElement('div');
    modalBackdrop.className = 'hj-modal-backdrop';
    modalBackdrop.setAttribute('aria-hidden', 'true');
    modalBackdrop.setAttribute('role', 'dialog');
    modalBackdrop.setAttribute('aria-modal', 'true');
    modalBackdrop.setAttribute('aria-labelledby', 'hj-activity-title');

    var modal = document.createElement('div');
    modal.className = 'hj-modal';
    modalBackdrop.appendChild(modal);

    var header = document.createElement('div');
    header.className = 'hj-modal-header';
    modal.appendChild(header);

    var title = document.createElement('h2');
    title.id = 'hj-activity-title';
    title.className = 'hj-modal-title';
    title.textContent = 'Activity History';
    header.appendChild(title);

    var closeBtn = document.createElement('button');
    closeBtn.id = 'hj-activity-close';
    closeBtn.className = 'hj-button hj-icon-button';
    closeBtn.type = 'button';
    closeBtn.setAttribute('aria-label', 'Close activity overview');
    closeBtn.textContent = 'x';
    header.appendChild(closeBtn);

    modal.appendChild(buildCalendarGrid(days));
    body.appendChild(modalBackdrop);

    openBtn.addEventListener('click', openModal);
    closeBtn.addEventListener('click', closeModal);
    modalBackdrop.addEventListener('click', function (ev) {
      if (ev.target === modalBackdrop) closeModal();
    });
    document.addEventListener('keydown', function (ev) {
      if (ev.key === 'Escape' && modalBackdrop.classList.contains('hj-open')) closeModal();
    });
  }

  initTheme();
  buildHeader();
  buildActivity(collectJournalDays());
  window.setTimeout(cleanupLegacyBearblog, 0);
  window.setTimeout(cleanupLegacyBearblog, 150);
});
