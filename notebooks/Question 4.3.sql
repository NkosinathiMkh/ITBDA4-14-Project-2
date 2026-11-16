-- Question 4.3: Verify database connection
SELECT current_database();
-- Question 4.3: Creating a relational table

-- Creating the table for the cleaned quotation dataset
CREATE TABLE quotes_cleaned (
    record_id INTEGER PRIMARY KEY,
    quote_text TEXT,
    author TEXT,
    tags TEXT,
    word_count INTEGER,
    source_page TEXT
);

-- Viewing the table structure and records
SELECT * FROM quotes_cleaned;

-- Question 4.3: Verify imported records

SELECT COUNT(*) AS total_records
FROM quotes_cleaned;

-- Question 4.3: SQL Analytical Investigation 1

-- Calculating the number of quotations and average word count per author
SELECT
    author,
    COUNT(*) AS total_quotes,
    ROUND(AVG(word_count), 2) AS average_word_count
FROM quotes_cleaned
GROUP BY author
ORDER BY average_word_count DESC;

-- Question 4.3: SQL Analytical Investigation 2

-- Classify quotations according to their word count
SELECT
    CASE
        WHEN word_count <= 20 THEN 'Short'
        WHEN word_count <= 50 THEN 'Medium'
        ELSE 'Long'
    END AS quote_length,
    COUNT(*) AS total_quotes,
    ROUND(AVG(word_count), 2) AS average_word_count,
    MIN(word_count) AS minimum_words,
    MAX(word_count) AS maximum_words
FROM quotes_cleaned
GROUP BY
    CASE
        WHEN word_count <= 20 THEN 'Short'
        WHEN word_count <= 50 THEN 'Medium'
        ELSE 'Long'
    END
ORDER BY minimum_words;