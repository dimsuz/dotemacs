;; From straight.el README.md:
;; Users of Emacs versions >= 27 will want to add [this setq]
;; to their early init-file to prevent package.el loading packages
;; prior to their init-file loading.
;;
;; The same is true for Elpaca
(setq package-enable-at-startup nil)
