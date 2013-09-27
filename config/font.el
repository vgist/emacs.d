;;;;;;;;;;;;;;;;;;;;;;;;;;; Linux环境下字体设置开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-font "DejaVu Sans Mono-10")
(set-fontset-font (frame-parameter nil 'font)
                  'han '("WenQuanYi Micro Hei" . "unicode-bmp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Linux环境下字体设置结束 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
