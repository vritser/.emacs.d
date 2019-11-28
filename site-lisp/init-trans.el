(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key (kbd "C-c y") 'google-translate-at-point)
(global-set-key (kbd "C-c Y") 'google-translate-query-translate)



(eval-after-load 'google-translate-core
  '(setq google-translate-base-url "https://translate.google.cn/translate_a/single"
         google-translate-listen-url "https://translate.google.cn/translate_tts"
         google-translate-default-target-language "zh-CN"
         google-translate-default-source-language "en"))

(eval-after-load 'google-translate-tk
  '(setq google-translate--tkk-url "https://translate.google.cn/"))

(provide 'init-trans)
