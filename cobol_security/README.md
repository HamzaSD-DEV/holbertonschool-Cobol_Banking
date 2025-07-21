Not empthy!

Yes! The original Task 1 asks for **password masking with asterisks**, which is **not natively supported in COBOL** on most platforms because COBOL does not have direct control over console input at the character level.

We can simplify the task while keeping the **main learning goals** (authentication, login attempts, feedback). Here’s the **modified version**:

---

### ✅ Simplified Task 1: Secure Login with Limited Attempts (COBOL-Compatible)

**Scenario**
Banking systems require basic security measures to protect customer accounts. In this task, you will implement a **login system with limited attempts and proper feedback**, using COBOL's standard capabilities.

---

#### **Your Program Should:**

1. Display a **Banking Login System** header.
2. Prompt the user to enter:

   * **User ID**
   * **Password** (input normally, no masking required because COBOL doesn't handle character-by-character masking easily).
3. Search for the User ID in the file `USERS.DAT`.
4. Validate the password.
5. **Limit login attempts to 3**. After three failed attempts, the program exits with a message.
6. Show **remaining attempts** after each failed attempt.
7. Provide clear success/failure messages.

---

#### **File: `USERS.DAT`**

Each line contains:

```
User ID     (8 chars)
User Name   (20 chars)
Password    (8 chars)
AccessLvl   (1 char)
```

**Example:**

```
USER001 John Smith         PASS123 1
USER002 Alice Johnson      SECRET2 2
USER003 Michael Brown      SECURE3 3
```

---

#### **Expected Behavior**

```
BANKING LOGIN SYSTEM
====================

Enter User ID: USER001
Enter Password: PASS123

Login successful!
Welcome, John Smith
Access level: 1
```

If invalid password:

```
BANKING LOGIN SYSTEM
====================

Enter User ID: USER001
Enter Password: WRONGPWD
Attempts remaining: 2

Invalid password. Authentication failed.
Press Enter to continue...
```

If user not found:

```
BANKING LOGIN SYSTEM
====================

Enter User ID: USER999
Enter Password: ANYTHING
Attempts remaining: 3

User not found.
Press Enter to continue...
```

After 3 failures:

```
Too many failed login attempts. Exiting...
```

---

✅ This is **fully doable in COBOL** using simple sequential file handling, ACCEPT/DISPLAY for input/output, and a **PERFORM loop with a counter**.

---

Do you want me to:
✔️ Write the **full COBOL solution for this simplified Task 1** (with login attempts and user validation, no masking)?
Or
✔️ Give you a **template + explain how to implement step by step**?
