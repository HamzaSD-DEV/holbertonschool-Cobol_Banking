Here's a **reformulated version of Task 6** with clearer, stricter validation requirements and a loop-based structure:

---

### ✅ **Task 6 – Robust Customer Input Validation with Loop Until Correct**

In real-world business applications, it's critical to guarantee that every piece of data collected from users is complete and valid before storing it. In this task, you'll create a COBOL program that **repeatedly prompts the user** for valid input and **only writes to `CUSTOMERS.DAT` once all fields are correct**.

---

### 🎯 **Objective:**

Write a COBOL program that prompts the user to enter **customer details**, validates each field in a loop until valid, then writes the cleaned data to a sequential file.

---

### 📥 **User Input Required:**

Prompt the user interactively for:

* **Account Number**:
  Must be a **5-digit numeric**, **greater than 0**, and **unique** (not already in `CUSTOMERS.DAT`).
* **First Name and Last Name**:
  Both required. Each must be:

  * Non-empty
  * Alphabetic only (no digits/symbols)
  * Exactly 10 characters (use spaces for padding if shorter)
* **Starting Balance**:
  Must be:

  * Non-empty
  * Numeric (with up to 2 decimal places)
  * Between **0.00** and **99999.99** (inclusive)

---

### ✅ **Validation Rules:**

* Loop and re-prompt **each field separately** until valid.
* Show a helpful error message when the user enters invalid data.
* **Only write the record** once all fields are valid.
* Display success message after write:
  ➤ `Account <account number> has been successfully added.`

---

### 💾 **File Output Format (`CUSTOMERS.DAT`)**

Each line must contain:

* 5-digit account number (zero-padded)
* 10-character First Name
* 10-character Last Name
* 8-character balance (zero-padded with 2 decimals)

Example line:

```
12345John      Doe       00500.00
```

---

### 🛠 **Hints:**

* Use **`PERFORM UNTIL`** for validation loops.
* Use **`FUNCTION NUMVAL`**, **`FUNCTION LENGTH`**, and **`INSPECT`** for input checks.
* Use **`READ`** and **`SEARCH`** to verify uniqueness.
* Use **`WRITE`** to add to the file after validation.

---

Would you like me to write the full COBOL program based on this reformulated task?
