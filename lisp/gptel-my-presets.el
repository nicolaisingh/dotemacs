;;; gptel-my-presets.el --- Presets for gptel

;; Copyright (C) 2025 Nicolai Singh

;; Author: Nicolai Singh <nicolaisingh at pm.me>
;; Created 24 Nov 2025
;; Version: 1.0
;; Keywords: gptel
;; Homepage: https://github.com/nicolaisingh/dotemacs

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

(require 'gptel)

;;;; Models

(gptel-make-preset 'model-deepseek-chat
  :description "Use DeepSeek chat (general conversation, basic tasks)"
  :backend "DeepSeek"
  :model 'deepseek-chat)

(gptel-make-preset 'model-deepseek-reasoner
  :description "Use DeepSeek reasoner (thinking, coding, reasoning, math)"
  :backend "DeepSeek"
  :model 'deepseek-reasoner)

(gptel-make-preset 'model-gpt-4.1
  :description "Use ChatGPT gpt-4.1 (complex tasks)"
  :backend "ChatGPT"
  :model 'gpt-4.1)

(gptel-make-preset 'gpt-5
  :description "Use ChatGPT gpt-5 (coding, reasoning, agentic)"
  :backend "ChatGPT"
  :model 'gpt-5)

(gptel-make-preset 'o4-mini
  :description "Use ChatGPT o4-mini (fast, reasoning, coding, visual)"
  :backend "ChatGPT"
  :model 'o4-mini)

;;;; Temperatures

(gptel-make-preset 'temp-coding
  :description "Temperature for coding"
  :temperature 0.0)

(gptel-make-preset 'temp-data
  :description "Temperature for data cleaning/analysis"
  :temperature 1.0)

(gptel-make-preset 'temp-conversation
  :description "Temperature for conversations/translations."
  :temperature 1.3)

(gptel-make-preset 'temp-creative-writing
  :description "Temperature for creative writing like poetry."
  :temperature 1.5)

;;;; System prompts

(gptel-make-preset 'coder-deepseek
  :description "Generate/Intepret code (deepseek-chat)"
  :system 'programming
  :backend "DeepSeek"
  :model 'deepseek-reasoner
  :temperature 0.0)

(gptel-make-preset 'coder-o4-mini
  :description "Generate/Intepret code (o4-mini)"
  :system 'programming
  :backend "ChatGPT"
  :model 'o4-mini
  :temperature 0.0)

(gptel-make-preset 'localize-en-fil
  :description "English-Filipino app localization"
  :system 'localize-en-fil
  :include-reasoning nil
  :use-context nil)

(gptel-make-preset 'prompt-maker
  :description "Write AI prompts"
  :system 'prompter
  :backend "DeepSeek"
  :model 'deepseek-chat
  :temperature 1.5)

(gptel-make-preset 'proofread
  :description "Proofread text"
  :system 'proofread
  :include-reasoning nil
  :use-context nil)

(gptel-make-preset 'summarize
  :description "Summarize text"
  :system 'summarizer
  :backend "ChatGPT"
  :model 'gpt-4.1-mini)


(provide ' gptel-my-presets)
;;; gptel-my-presets.el ends here
