;;; init-agent-shell.el --- LLM agent shell.	-*- lexical-binding: t -*-

;; Copyright (C) 2024  vritser

;; Author: vritser <vritser@gmail.com>
;; Keywords: extensions, processes, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A native Emacs shell to interact with LLM agents powered by ACP
;; (Agent Client Protocol). Supports Claude Agent, Gemini CLI, Codex,
;; Goose, Cursor, and Pi.
;;
;; https://github.com/xenodium/agent-shell
;;
;; Prerequisites:
;;   Install at least one ACP agent CLI, e.g.:
;;     npm install -g @agentclientprotocol/claude-agent-acp
;;     npm install -g pi-acp
;;
;; Quick start:
;;   M-x agent-shell   -- start or reuse a known agent (default: Pi)
;;   M-x agent-shell-pi-start-agent         -- Pi coding agent
;;   M-x agent-shell-anthropic-start-claude-code  -- Claude Agent
;;

;;; Code:

(use-package agent-shell
  :ensure t
  :defer t
  :defines (acp-make-client
            agent-shell-preferred-agent-config
            agent-shell-anthropic-authentication
            agent-shell-anthropic-claude-environment
            agent-shell-pi-environment
            agent-shell-pi-acp-command
            agent-shell-show-context-usage-indicator
            agent-shell-mode-map)
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code
             agent-shell-google-start-gemini
             agent-shell-openai-start-codex
             agent-shell-pi-start-agent)

  :init
  ;; Simple boolean flags (no package functions needed)
  (setq agent-shell-show-context-usage-indicator t)

  :hook
  ;; Disable page-break-lines in agent-shell buffers (conflicts with the UI)
  (agent-shell-mode . (lambda ()
                        (when (fboundp 'page-break-lines-mode)
                          (page-break-lines-mode -1))))

  :bind
  (:map agent-shell-mode-map
        ;; Customize RET behaviour: RET inserts newline, M-RET submits
        ("RET" . newline)
        ("M-RET" . agent-shell-submit))

  :config
  ;; NOTE: Everything below calls package functions, so it must be in
  ;; `:config' (runs after the package loads), NOT in `:init'.
  (declare-function agent-shell-make-environment-variables "agent-shell")
  (declare-function agent-shell-anthropic-make-authentication "agent-shell")
  (declare-function agent-shell-pi-make-agent-config "agent-shell")

  ;; Default agent for M-x agent-shell
  (setq agent-shell-preferred-agent-config
        (agent-shell-pi-make-agent-config))

  ;; Claude authentication and env (login-based)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  ;; Pi / pi-acp: pi handles its own API keys internally, no auth setup needed.
  ;; Inherit Emacs env so pi finds the same PATH, models, etc.
  (setq agent-shell-pi-environment
        (agent-shell-make-environment-variables :inherit-env t)))

(provide 'init-agent-shell)

;;; init-agent-shell.el ends here