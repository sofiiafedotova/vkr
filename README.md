# Приложение к дипломной работе "Соотнесение текстовой кластеризации с пространственным распределением русского старожильческого населения монголии (на материале этнографических интервью)": данные и код

В дипломной работе исследуются закономерности в соотношении распределения текстов и их сюжетно-структурных особенностей с пространственным распределением русских старожилов Монголии. Данными служат мифологические рассказы (былички), записанные в ходе этнографических интервью с русскими Монголии в 2018–2024 гг. Поиск закономерностей в векторизованных посредством TF-IDF текстах и в таблицах с категориальными признаками быличек осуществлялся методом кластеризации. Проинтерпретированы результаты применения алгоритмов; проведены дополнительные исследования с задействованием сравнительного корпуса быличек из Забайкалья, а также с использованием точного теста Фишера; проанализирована связь выявленных закономерностей с локализацией описанных в текстах событий.

Структура GitHub соответствует структуре выпускной квалификационной работы: в папке "Кластеризация TF-IDF" опубликованы материалы для воспроизводимости результатов Главы II "Кластерный анализ текстов", в папке "Кластеризация категориальных признаков -- главы III "Анализ категориальных признаков".

## Состав папки "Кластеризация TF-IDF"

(-) В папке ["Корпуса"](https://github.com/sofiiafedotova/vkr/tree/main/%D0%9A%D0%BB%D0%B0%D1%81%D1%82%D0%B5%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F%20TF-IDF/%D0%9A%D0%BE%D1%80%D0%BF%D1%83%D1%81%D0%B0) содержатся: 

~ Основной корпус быличек (необходимо запросить доступ по ссылке: тексты планируются к публикации)

~ Экспериментальный корпус быличек (собиратель: В.П. Зиновьев)

~ Книга В.П. Зиновьева "Русский фольклор..." (источник текстов из Экспериментального корпуса)

(-) В папке ["R"](https://github.com/sofiiafedotova/vkr/tree/main/%D0%9A%D0%BB%D0%B0%D1%81%D1%82%D0%B5%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F%20TF-IDF/R) содержится пошаговый код для кластеризации текстов алгоритмами k-means и DBSCAN на R и сопутствующего анализа (предобработка основного корпуса, виузализация карты пространственных меток, обработка вектора именованных сущностей, собственно кластеризация k-means и DBSCAN, рассчет IMP, предобработка и кластеризация экспериментального корпуса). Файлы необходимо запускать в указанном порядке, не очищая окружение.

(-) В папке ["Python"](https://github.com/sofiiafedotova/vkr/tree/main/%D0%9A%D0%BB%D0%B0%D1%81%D1%82%D0%B5%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F%20TF-IDF/Python) содержится IPYNB-тетрадка с кодом для кластеризации основного и сводного корпусов посредством библиотеки scikit-learn и предобработанные в R таблицы ("stem_clean.xlsx" - Основной корпус, "experiment.xlsx" - Сводный корпус) для корректного запуска кода.

## Состав папки "Кластеризация TF-IDF"
 В папке содержится пошаговый  код для кластеризации категориальных признаков быличек (необходимо запускать в указанном порядке, не очищая окружение) и таблицы с категориальными признаками ("Разметка.xlsx" - в машиночитаемом виде (для выполнения файла с кодом 2. ...; "Разметка (человекочитаемый вид).xlsx" - в удобном для чтения виде).

## Благодарности

Исследование выполнено в рамках проекта РНФ №23-18-00478 «Русские Монголии. Комплексное исследование культуры в иноэтническом окружении (фольклор, обрядовые традиции, язык)».

## Контакты

Вопросы и предложения прошу направлять на общую почту проекта: rus-mongol.imli@inbox.ru
