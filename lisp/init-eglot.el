;;; init-eglot.el --- Eglot configuration (alternative to init-lsp.el). -*- lexical-binding: t -*-

;; Copyright (C) 2026  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; 基于内置 Eglot 的 LSP 配置，与 init-lsp.el 二选一使用。
;;
;; ## 启用方式（不要与 init-lsp.el 同时加载，两个 LSP 客户端会互相冲突）
;;
;; 1. 临时试用（不动 init.el）:
;;    M-x load-file RET ~/.emacs.d/lisp/init-eglot.el
;;    然后 M-x eglot 手动连接，或 M-x eval-buffer 让 hook 生效后重开文件。
;; 2. 正式切换:
;;    把 init.el 中的 (require 'init-lsp) 换成 (require 'init-eglot)。
;;
;; ## 需要自行安装的语言服务器
;;
;;    Go:     gopls                   （你已装有，无需动作）
;;    JS/TS:  typescript-language-server   (npm i -g typescript-language-server)
;;    Java:   jdtls                   （lsp-java 时代下载到
;;                                      ~/.emacs.d/.cache/lsp/java/eclipse.jdt.ls/
;;                                      PATH 里有 jdtls 也可）
;;    Scala:  metals                  （eglot 默认探测器自动找 metals/metals-emacs）
;;    Python: pylsp / pyright         （eglot 默认探测器自动找）
;;    JSON/CSS: vscode-*-language-server（可选，eglot 默认探测器自动找）
;;
;; ## 与 lsp-mode 的行为差异
;;
;;    - 诊断走内置 flymake（不是 flycheck）；flycheck 仍可保留跑 eslint 等非 LSP 检查。
;;    - 无 lsp-ui-peek：定义/引用跳转回落到内置 xref（eglot 自动接管 M-./M-?）。
;;    - 代码操作（code action）用 M-RET，organize imports 用
;;      eglot-code-action-organize-imports（Emacs 30+ 自带）。
;;    - inlay hints 默认关闭，需要时 M-x eglot-inlay-hints-mode。
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(declare-function eglot-format-buffer "eglot" () t)

(use-package eglot
  :ensure nil                ; 内置包，别让 use-package-always-ensure 去 MELPA 装
  :defer t
  :hook ((prog-mode . (lambda ()
                        ;; 与原 lsp-mode 的 hook 对齐：排除 elisp 自身
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (eglot-ensure)))))
  :bind (:map eglot-mode-map
              ("M-RET" . eglot-code-actions)
              ("C-c C-d" . eldoc)                     ; 替代 lsp-describe-thing-at-point
              ("C-c u" . eglot-imenu))                ; 替代 lsp-ui-imenu
  :init
  (setq read-process-output-max (* 1024 1024)
        eglot-autoshutdown t)      ; 项目 buffer 全关后自动下线服务器

  :config
  ;; 默认列表已覆盖 js-mode/js-ts-mode/typescript-mode/css/java/go/python(scala 也有)，
  ;; 唯独缺 js2-mode（js2-mode 不派生自 js-mode），补上并指定 language-id。
  (add-to-list 'eglot-server-programs
               '(((js2-mode :language-id "javascript"))
                 . ("typescript-language-server" "--stdio")))
  ;; Java 用自定义启动函数：优先 PATH 里的 jdtls，其次 lsp-java 的缓存目录
  (add-to-list 'eglot-server-programs
               '(java-mode . my-eglot-jdtls-command))

  ;; 对齐原 lsp-mode 的保存时格式化（尊重 centaur-lsp-format-on-save-ignore-modes）
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                (add-hook 'before-save-hook #'eglot-format-buffer t t)))))

(defvar my-eglot-lombok-agent nil
  "Lombok jar 路径（如 ~/.m2/.../lombok-1.18.32.jar）。
非 nil 时作为 -javaagent 传给 jdtls。")

(defun my-eglot-jdtls-command (&optional _interactive _project)
  "Return the jdtls command line for Eglot.

优先使用 PATH 中的 jdtls；否则回退到 lsp-java 时代下载的
~/.emacs.d/.cache/lsp/java/eclipse.jdt.ls/<version>/bin/jdtls。
工作区（-data）放在 ~/.emacs.d/jdtls-workspace/。

jdtls 未安装时发出明确的安装提示（而不是静默返回 nil）。"
  (let* ((home "~/.emacs.d/.cache/lsp/java/eclipse.jdt.ls")
         (jdtls (or (executable-find "jdtls")
                    (car (file-expand-wildcards
                          (expand-file-name "*/bin/jdtls" home)))))
         (workspace (expand-file-name "jdtls-workspace/" user-emacs-directory)))
    (if jdtls
        (append (list jdtls "-data" workspace)
                (and my-eglot-lombok-agent
                     (list "-javaagent:" my-eglot-lombok-agent)))
      (user-error "jdtls 未找到。请安装 jdtls（brew install jdtls 或 \
下载 eclipse.jdt.ls），或将其加入 PATH / ~/.emacs.d/.cache/lsp/java/eclipse.jdt.ls/<version>/bin/"))))

(provide 'init-eglot)
;;; init-eglot.el ends here