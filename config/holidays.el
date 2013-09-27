;; 节日
(setq calendar-latitude 31.22)
(setq calendar-longitude 121.48)
(setq calendar-location-name "Shanghai")

;; 设置阴历显示，在 calendar 上用 pC 显示阴历
(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])

;; 去掉不关心的节日，设定自己在意的节日，在 calendar 上用 h 显示节日
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq solar-holidays nil)
(setq general-holidays '((holiday-fixed 1 1 "元旦")
                         (holiday-fixed 2 14 "情人节")
                         (holiday-fixed 3 14 "白色情人节")
                         (holiday-fixed 4 1 "愚人节")
                         (holiday-fixed 5 1 "劳动节")
                         (holiday-float 5 0 2 "母亲节")
                         (holiday-fixed 6 1 "儿童节")
                         (holiday-float 6 0 3 "父亲节")
                         (holiday-fixed 7 1 "建党节")
                         (holiday-fixed 8 1 "建军节")
                         (holiday-fixed 9 10 "教师节")
                         (holiday-fixed 10 1 "国庆节")
                         (holiday-fixed 12 25 "圣诞节")))

;; EMACS Calendar支持配置节日：
;; h 显示当前的节日
;; x 定义当天为某个节日
;; u 取消当天已被定义的节日
;; e 显示所有这前后共三个月的节日。
;; M-x holidays 在另外的窗口的显示这前后三个月的节日。
