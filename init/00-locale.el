(provide '00-locale)

;;;;;;;;;;;;;;;;;;;;;;;;;;; 字体设置开始 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'ctext)

(when (eq system-type 'linux)
  (set-frame-font "DejaVu Sans Mono-11")
  (set-fontset-font "fontset-default"
                    'unicode '("WenQuanYi Micro Hei" . "unicode-bmp"))
)

(when (eq system-type 'darwin)
  (set-frame-font "Monaco-11")
  (set-fontset-font t 'unicode (font-spec :name "STHeiti"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;; 字体设置结束 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
