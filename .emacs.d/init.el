;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
          (expand-file-name (concat user-emacs-directory path))))
          (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "elpa")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Linux 特化の設定
(when (equal system-type 'gnu/linux)
  (load-file "~/.emacs.d/init-linux.el"))

;; Windows 特化の設定
(when (equal system-type 'windows-nt)
  (load-file "~/.emacs.d/init-windows.el"))

;; auto-complete.el
(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
(require 'auto-complete-config)
(ac-config-default)
;; C-n/C-pで候補選択
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)

;; js2-mode
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; C-Enterで短形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; "C-t" でウィンドウを切り替える。初期値はtranspose-chars
(global-set-key (kbd "C-t") 'other-window)

;; "C-h" でBackspaceキー相当の削除をする。初期値はhelp表示
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Welcomeメッセージを非表示にする
(setq inhibit-startup-message t)

;; scratchの初期メッセージ消去する
(setq initial-scratch-message "")

;; 行番号を表示する
(global-linum-mode t)

;; 検索(全般)時には大文字小文字の区別をしない
(setq case-fold-search t)

;; temp ファイル類を作成しないようにする
;; http://masutaka.net/chalow/2014-05-11-1.html
;;; filename~ のバックアップファイルを作成しないようにする
(setq make-backup-files nil)

(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; web-mode
(require 'web-mode)

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
;;; auto indentation on
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq web-mode-attr-indent-offset 2))
(add-hook 'web-mode-hook 'web-mode-hook)

;;; Autoremove final white spaces on save
(add-hook 'local-write-file-hooks
            (lambda ()
               (delete-trailing-whitespace)
               nil))

(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)

;; php-mode
(require 'php-mode)

;;; カラーテーマ設定
;;; http://aoe-tk.hatenablog.com/entry/20130210/1360506829
(load-theme 'manoj-dark t)

;;
;; whitespace
;;
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)

;; multi-term の設定
;;; 新しい Emacs でうまく動かないのでコメントアウトして無効化
;(when (require 'multi-term nil t)
  ;; 使用するシェルを指定
;  (setq multi-term-program "/bin/bash"))

;; TRAMP (local から remote ファイルを編集機能 C-x C-f /sshx:user@host:)
;;; バックアップファイルを作成しない設定
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; pyflakes (Python リアルタイム構文チェッカー）
;;; http://yukke.hateblo.jp/entry/2015/01/09/222311
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pyflakes"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
; show message on mini-buffer
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flymake-python-pyflakes yaml-mode web-mode php-mode markdown-mode js2-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
