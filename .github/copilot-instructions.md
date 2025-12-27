# Copilot/AI Agent Instructions for cl-git-tree

## Архитектура и основные компоненты
- **cl-git-tree** — инструмент для управления git-локациями и синхронизации репозиториев на Common Lisp.
- Ключевые директории:
  - `src/commands/` — отдельные CLI-команды (add, commit, pull, push, remote-*, locations, patterns, audit и др.).
  - `src/loc/` — классы и API для локаций и провайдеров (`<location>`, `<provider>`, `<local>`, `<github>`, `<gitlab>`).
  - `src/dispatch/` — диспетчер CLI-команд, глобальный реестр `*commands*`.
  - `src/fs/`, `src/git-utils/`, `src/shell-utils/` — утилиты для работы с файловой системой, git и shell.
  - `src/config/` — загрузка конфигурации и шаблонов файлов.
  - `tests/` — модульные тесты для всех основных компонентов.

## Ключевые паттерны и соглашения
- **Реестры**: команды и локации регистрируются глобально (`*commands*`, `*locations*`).
- **Полиморфизм**: методы для провайдеров реализуются через отдельные файлы в `src/loc/methods/`.
- **CLI-команды**: добавляются как отдельные файлы, регистрируются через `register-command`.
- **Конфигурация**: локации и паттерны файлов хранятся в `~/.git-tree/locations.lisp` и `~/.git-tree/file-patterns.lisp`.
- **Тесты**: запуск через SBCL, пример:
  ```bash
  sbcl --eval "(progn (require 'asdf) (asdf:load-system :cl-git-tree-tests) (uiop:symbol-call :cl-git-tree/tests :run-tests))" --quit
  ```

## Важные команды и сценарии
- **Загрузка системы**:
  ```lisp
  (ql:quickload "cl-git-tree")
  ```
- **CLI-использование** (через SBCL):
  ```bash
  sbcl --eval "(progn (require 'asdf) (asdf:load-system :cl-git-tree) (cl-git-tree/cli:main '(\"prog\" \"--help\")))" --quit
  ```
- **Добавление новой команды**:
  1. Создать файл в `src/commands/` с функцией `cmd-<name>`.
  2. Зарегистрировать через `register-command`.
  3. Добавить в `.asd`-файл.

## Особенности
- **Глобальное состояние**: избегайте явной передачи глобальных переменных, используйте реестры.
- **Пути с тильдой**: автоматически разворачиваются в `$HOME`.
- **Офлайн-транспорт**: экспорт/импорт репозиториев через tar.xz (`git tree transport export/import`).
- **Покрытие тестами**: все основные модули имеют тесты в `tests/`.

## Примеры
- **Регистрация локации** (Lisp):
  ```lisp
  (cl-git-tree/loc:add-location "gh" :description "GitHub" :url-git "git@github.com:username/" :provider :github)
  ```
- **Клонирование репозитория** (CLI):
  ```bash
  sbcl --eval "(progn (require 'asdf) (asdf:load-system :cl-git-tree) (cl-git-tree/cli:main '(\"prog\" \"clone\" \"gh\" \"cl-git-tree\")))" --quit
  ```

## См. также
- `README.org` — подробная документация, архитектура, примеры.
- `src/cli.lisp`, `src/dispatch/dispatcher.lisp`, `src/loc/location.lisp` — ключевые точки входа и API.
- `tests/` — примеры тестирования и сценарии использования.

## Особенности путей и MSYS2/Windows
- Пути к архивам и репозиториям не должны заканчиваться слэшем, особенно при операциях импорта/экспорта.
- На Windows/MSYS2 используйте прямые слэши (`/`) для совместимости, избегайте обратных слэшей (`\\`).
- Тильда (`~`) автоматически разворачивается в `$HOME`.
- При ошибках распаковки архивов через `git tree transport import` анализируйте сообщения tar и убедитесь, что архив существует и путь корректен.
- Не прерывайте обработку других архивов при ошибках — выводите причину и продолжайте.
