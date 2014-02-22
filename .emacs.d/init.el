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
(add-to-load-path "elisp" "conf" "public_repos" "elpa")

;; https://github.com/emacs-jp/init-loader
;; 設定ファイルを分割する拡張
;; (require 'init-loader)
;; (init-loader-load "~/.emacs.d/conf")

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

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
