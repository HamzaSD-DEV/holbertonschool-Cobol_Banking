exec_bash_contains("printf \"ADMIN010\nA\nRD\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nRD\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nRD\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nRD\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nRD\n5\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nWR\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nWR\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nWR\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nWR\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nWR\n5\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nDL\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nDL\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nDL\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nDL\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nDL\n5\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nAU\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nAU\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nAU\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nAU\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nAU\n5\n\" | ./rbac-security" , ["GRANTED"]) 0. Implement Structured Error Handling in a COBOL Script
Level: 0
Auto review
In real-world COBOL systems, especially in banking or government environments, predictable error handling is critical. Whether it’s dealing with missing records or invalid user input, COBOL provides structured ways to detect and respond to errors.

You are given with a fixed-length sequential file named CUSTOMERS.DAT containing customer IDs and names.

Customer ID: 5 digits
Customer Name: 20 characters
Write a COBOL program that:

Prompt the user to enter a Customer ID.
Search the file for the entered ID.
If the ID exists, display: Customer found: <Customer Name>
If not, handle the error and display: Error: Customer ID not found
Hints

Use sequential file access (READ/OPEN).
Use a loop to read records one by one and compare Customer ID.
Use READ file-name AT END to handle end-of-file cases.
┌──(maroua)-[~/cobol_files]
└─🏴cat CUSTOMERS.DAT
10001John Smith         
10002Alice Johnson      
10003Michael Lee        
┌──(maroua)-[~/cobol_files]
└─🏴 cobc -x 0-error_handling.cbl
┌──(maroua)-[~/cobol_files]
└─🏴./0-error_handling.cbl
Enter Customer ID: 10002  
Customer found: Alice Johnson
Enter Customer ID: 10005  
Error: Customer ID not found

exec_bash_contains("printf \"MANAGER01\nM\nRD\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nRD\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nRD\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nRD\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nRD\n5\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nWR\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nWR\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nWR\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nWR\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nWR\n5\n\" | ./rbac-security" , ["DENIED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nDL\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nDL\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nDL\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nDL\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nDL\n5\n\" | ./rbac-security" , ["DENIED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nAU\n1\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nAU\n2\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nAU\n3\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nAU\n4\n\" | ./rbac-security" , ["GRANTED"]) and
exec_bash_contains("printf \"MANAGER01\nM\nAU\n5\n\" | ./rbac-security" , ["GRANTED"]) 

exec_bash_contains("printf \"USER010\nU\nRD\n1\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nRD\n2\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nRD\n3\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nRD\n4\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nRD\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nWR\n1\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nWR\n2\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nWR\n3\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"USER010\nU\nWR\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nWR\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nDL\n1\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nDL\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nDL\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nDL\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nDL\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nAU\n1\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nAU\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nAU\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nAU\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"USER010\nU\nAU\n5\n\" | ./rbac-security"  , ["DENIED"]) 

exec_bash_contains("printf \"GUEST01\nG\nRD\n1\n\" | ./rbac-security"  , ["GRANTED"]) and
exec_bash_contains("printf \"GUEST01\nG\nRD\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nRD\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nRD\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nRD\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nWR\n1\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nWR\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nWR\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nWR\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nWR\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nDL\n1\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nDL\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nDL\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nDL\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nDL\n5\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nAU\n1\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nAU\n2\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nAU\n3\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nAU\n4\n\" | ./rbac-security"  , ["DENIED"]) and
exec_bash_contains("printf \"GUEST01\nG\nAU\n5\n\" | ./rbac-security"  , ["DENIED"]) 

exec_bash_contains("printf \"--\nA\nRD\n1\n\" | ./rbac-security" , ["DENIED"]) and
exec_bash_contains("printf \"ADMIN010\nZ\nRD\n2\n\" | ./rbac-security" , ["DENIED"]) and
exec_bash_contains("printf \"ADMIN010\nA\nZZ\n3\n\" | ./rbac-security" , ["DENIED"]) 