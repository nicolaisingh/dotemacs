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

;; DeepSeek

(gptel-make-preset 'deepseek-chat
  :description "Use DeepSeek chat"
  :backend "DeepSeek"
  :model 'deepseek-chat)

(gptel-make-preset 'deepseek-reasoner
  :description "Use DeepSeek reasoner"
  :backend "DeepSeek"
  :model 'deepseek-reasoner)

;; OpenAI

(gptel-make-preset 'gpt-5.4
  :description "OpenAI gpt-5.4"
  :backend "ChatGPT"
  :model 'gpt-5.4)

(gptel-make-preset 'gpt-5.4-mini
  :description "OpenAI gpt-5.4 mini"
  :backend "ChatGPT"
  :model 'gpt-5.4-mini)

(gptel-make-preset 'gpt-5.4-nano
  :description "OpenAI gpt-5.4 mini"
  :backend "ChatGPT"
  :model 'gpt-5.4-nano)

;; Ollama (Local)

(gptel-make-preset 'deepseek-coder-v2-16b
  :description "DeepSeek Coder V2 16B (MoE)"
  :backend "Ollama"
  :model 'deepseek-coder-v2:16b)

(gptel-make-preset 'gemma4-26b
  :description "Google Gemma 4 26B"
  :backend "Ollama"
  :model 'gemma4:26b)

(gptel-make-preset 'glm-4.7-flash
  :description "GLM 4.7 Flash"
  :backend "Ollama"
  :model 'glm-4.7-flash:latest)

(gptel-make-preset 'lfm2
  :description "Liquid LFM2 24B"
  :backend "Ollama"
  :model 'lfm2:latest)

(gptel-make-preset 'qwen3-coder
  :description "Alibaba Qwen 3 coder"
  :backend "Ollama"
  :model 'qwen3-coder:latest)

(gptel-make-preset 'translategemma-12b
  :description "Gemma 3-based translation model 12B"
  :backend "Ollama"
  :model 'translategemma:12b)

;; Ollama (Cloud)

(gptel-make-preset 'kimi-k2.5-cloud
  :description "Moonshot AI Kimi-K2.5"
  :backend "Ollama"
  :model 'kimi-k2.5:cloud)

(gptel-make-preset 'glm-5.1-cloud
  :description "Z.ai GLM 5.1"
  :backend "Ollama"
  :model 'glm-5.1:cloud)

;;;; Temperatures

(gptel-make-preset 'temp-coding
  :description "Temperature for coding"
  :temperature 0.2)

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
  :description "Generate/Intepret code (deepseek-coder-v2-16b)"
  :system 'programming
  :parents '(deepseek-coder-v2-16b temp-coding))

(gptel-make-preset 'coder-qwen3-coder
  :description "Generate/Intepret code (qwen3-coder)"
  :system 'programming
  :parents '(qwen3-coder temp-coding))

(gptel-make-preset 'localize-en-fil
  :description "English-Filipino app localization"
  :system 'localize-en-fil
  :include-reasoning nil
  :use-context nil
  :parents '(translategemma-12b temp-conversation))

(gptel-make-preset 'prompt-maker
  :description "Write AI prompts"
  :system 'prompter
  :parents '(glm-5.1-cloud temp-conversation))

(gptel-make-preset 'proofread
  :description "Proofread text"
  :system 'proofread
  :include-reasoning nil
  :use-context nil
  :parents '(gemma4-26b temp-conversation))

(gptel-make-preset 'rewrite
  :description "Rewrite text"
  :system 'rewrite
  :include-reasoning nil
  :use-context nil
  :parents '(gemma4-26b temp-conversation))

(gptel-make-preset 'summarize
  :description "Summarize text"
  :system 'summarizer
  :parents '(gemma4-26b temp-conversation))

(gptel-make-preset 'git-commit-o4-mini
  :description "Git commit assistant"
  :system 'git-commit
  :parents '(coder-o4-mini))

(gptel-make-preset 'git-commit
  :description "Git commit assistant"
  :system 'git-commit
  :parents '(kimi-k2.5-cloud))

(gptel-make-preset 'cli-assistant
  :description "CLI command assistant"
  :system 'cli-assistant
  :backend "Ollama"
  :model '(qwen3-coder))

(provide ' gptel-my-presets)
;;; gptel-my-presets.el ends here
