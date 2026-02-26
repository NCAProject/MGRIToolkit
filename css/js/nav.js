/* =====================================================
   MGRI Handbook — Sidebar & Nav Behavior
   ===================================================== */

(function () {
  'use strict';

  /* --- Mark active nav link based on current page --- */
  function setActiveLink() {
    const currentFile = window.location.pathname.split('/').pop() || 'index.html';
    document.querySelectorAll('#sidebar .nav-link').forEach(function (link) {
      const href = link.getAttribute('href');
      if (href === currentFile || href === './' + currentFile) {
        link.classList.add('active');
        // Scroll sidebar so active item is visible
        link.scrollIntoView({ block: 'nearest' });
      }
    });
  }

  /* --- Mobile sidebar toggle --- */
  function initMobileToggle() {
    const toggle = document.getElementById('sidebar-toggle');
    const sidebar = document.getElementById('sidebar');
    const main = document.getElementById('main');

    if (!toggle || !sidebar) return;

    toggle.addEventListener('click', function () {
      sidebar.classList.toggle('open');
      toggle.textContent = sidebar.classList.contains('open') ? '✕' : '☰';
    });

    // Close sidebar when clicking main content on mobile
    if (main) {
      main.addEventListener('click', function () {
        if (sidebar.classList.contains('open')) {
          sidebar.classList.remove('open');
          toggle.textContent = '☰';
        }
      });
    }

    // Close sidebar when a nav link is clicked on mobile
    sidebar.querySelectorAll('.nav-link').forEach(function (link) {
      link.addEventListener('click', function () {
        if (window.innerWidth <= 768) {
          sidebar.classList.remove('open');
          toggle.textContent = '☰';
        }
      });
    });
  }

  /* --- Smooth anchor scrolling for in-page links --- */
  function initSmoothScroll() {
    document.querySelectorAll('a[href^="#"]').forEach(function (anchor) {
      anchor.addEventListener('click', function (e) {
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
          e.preventDefault();
          target.scrollIntoView({ behavior: 'smooth', block: 'start' });
        }
      });
    });
  }

  /* --- Init --- */
  document.addEventListener('DOMContentLoaded', function () {
    setActiveLink();
    initMobileToggle();
    initSmoothScroll();
  });
})();