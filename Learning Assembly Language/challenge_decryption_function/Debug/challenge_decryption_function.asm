;=================================================================================================================
; challenge_decryption_function.asm
;=================================================================================================================

;=================================================================================================================
; Return my solution to "Challenge: Decryption Function" from the course "Learning Assembly Language" on Linkedin 
; Learning. Upon running, a menu queries the user for one of six jump-table options. "Test CPU AES Compatibility" 
; queries the CPU for AES capability and, if successful, will run the AES encryption algorithm on the test key and
; plaintext given in FIPS 197 and inform the user of the result. "Generate New Encryption Key" generates a random, 
; 16-byte hexadecimal key and outpust it to the human-readable file key.txt. "Read Encryption Key" will read any 
; 32-hexadecimal character file key.txt into the program memory for use in either encryption or decryption. 
; "Encrypt File" takes as input a file plaintext.txt and a key stored in the program memory and outputs an 
; encrypted file encrypted.txt. "Decrypt File" takes as input a file encrypted.txt and a key stored in the 
; program memory and outputs a decrypted file decrypted.txt. Finally, "Exit" closes open handles and exits the 
; running of the program.
;=================================================================================================================

.Const

GENERIC_READ             Equ     080000000h
GENERIC_WRITE            Equ     040000000h
CREATE_ALWAYS            Equ     2
OPEN_EXISTING            Equ     3
FILE_ATTRIBUTE_NORMAL    Equ     080h
INVALID_FILE_HANDLE      Equ     -1

.Data

h_inst        DQ    0
h_in          DQ    0
h_out         DQ    0
h_file_in     DQ    0
h_file_out    DQ    0

b_in       DQ    0
b_out      DQ    0
char_in    DB    020h    Dup    0

menu_msg                 DB    'AES-128 File Encryptor', 0Ah, 0Dh
                         DB    '    0...Exit', 0Ah, 0Dh
                         DB    '    1...Test CPU AES Compatibility', 0Ah, 0Dh
                         DB    '    2...Generate New Encryption Key', 0Ah, 0Dh
                         DB    '    3...Read Encryption Key', 0Ah, 0Dh
                         DB    '    4...Encrypt File', 0Ah, 0Dh
                         DB    '    5...Decrypt File', 0Ah, 0Dh
                         DB    '> '
menu_len                 DQ    $-menu_msg
menu_err_msg             DB    'ERROR: Input not in range. Please try again.', 0Ah, 0Dh
menu_err_len             DQ    $-menu_err_msg
aes_false_msg            DB    'ERROR: AES not supported by CPU.', 0Ah, 0Dh
aes_false_len            DQ    $-aes_false_msg
aes_true_msg             DB    'AES supported by CPU.', 0Ah, 0Dh
aes_true_len             DQ    $-aes_true_msg
cipher_test_true_msg     DB    'Cipher test successful.', 0Ah, 0Dh
cipher_test_true_len     DQ    $-cipher_test_true_msg
cipher_test_false_msg    DB    'ERROR: Cipher test failed.', 0Ah, 0Dh
cipher_test_false_len    DQ    $-cipher_test_false_msg
gen_key_msg              DB    'Please enter an upper-case letter for the seed:', 0Ah, 0Dh, '> '
gen_key_len              DQ    $-gen_key_msg
gen_key_seed             DQ    0
key_filename             DB    'key.txt', 0
key_byte_out             DB    0
key_out_file_err_msg     DB    'ERROR: Could not create key.txt', 0Ah, 0Dh
key_out_file_err_len     DQ    $-key_out_file_err_msg
key_out_file_true_msg    DB    'New ecryption key succesfully generated and stored in key.txt', 0Ah, 0Dh
key_out_file_true_len    DQ    $-key_out_file_true_msg
key_in_file_err_msg      DB    'ERROR: No human-readable 32-hexadecimal number file key.txt in folder.', 0Ah, 0Dh
key_in_file_err_len      DQ    $-key_in_file_err_msg
key_in_file_true_msg     DB    'Key successfully read into program memory.', 0Ah, 0Dh
key_in_file_true_len     DQ    $-key_in_file_true_msg
plaintext_filename       DB    'plaintext.txt', 0
plain_file_err_msg       DB    'ERROR: plaintext.txt is not in folder.', 0Ah, 0Dh
plain_file_err_len       DQ    $-plain_file_err_msg
enc_filename             DB    'encrypted.txt', 0
key_in_enc_err_msg       DB    'ERROR: key.txt has not been read into program memory.', 0Ah, 0Dh
key_in_enc_err_len       DQ    $-key_in_enc_err_msg
enc_file_out_err_msg     DB    'ERROR: Could not create file encrypted.txt', 0Ah, 0Dh
enc_file_out_err_len     DQ    $-enc_file_out_err_msg
enc_file_true_msg        DB    'plaintext.txt successfully encrypted and stored in encrypted.txt', 0Ah, 0Dh
enc_file_true_len        DQ    $-enc_file_true_msg
enc_file_in_err_msg      DB    'ERROR: encrypted.txt is not in folder.', 0Ah, 0Dh
enc_file_in_err_len      DQ    $-enc_file_in_err_msg
dec_filename             DB    'decrypted.txt', 0
dec_file_out_err_msg     DB    'ERROR: Could not create decrypted.txt', 0Ah, 0Dh
dec_file_out_err_len     DQ    $-dec_file_out_err_msg
dec_file_true_msg        DB    'encrypted.txt successfully decrypted and stored in decrypted.txt', 0Ah, 0Dh
dec_file_true_len        DQ    $-dec_file_true_msg

jump_table    DQ    Addr exit
              DQ    Addr test_cpu
              DQ    Addr genr_key
              DQ    Addr store_key
              DQ    Addr encrypt
              DQ    Addr decrypt

Align 010h
plain_txt_test     DB    06Bh, 0C1h, 0BEh, 0E2h, 02Eh, 040h, 09Fh, 096h, 0E9h, 03Dh, 07Eh, 011h, 073h, 093h, 017h, 02Ah
cipher_test        DB    03Ah, 0D7h, 07Bh, 0B4h, 00Dh, 07Ah, 036h, 060h, 0A8h, 09Eh, 0CAh, 0F3h, 024h, 066h, 0EFh, 097h
enc_key_test       DB    02Bh, 07Eh, 015h, 016h, 028h, 0AEh, 0D2h, 0A6h, 0ABh, 0F7h, 015h, 088h, 009h, 0CFh, 04Fh, 03Ch
enc_exp_test       DB    0A0h    Dup    0
enc_key_out        DB    010h    Dup    0 
enc_key_in         DB    010h    Dup    0
enc_exp_in         DB    0A0h    Dup    0
plaintext_in       DB    010h    Dup    0
enc_out            DB    010h    Dup    0
enc_in             DB    010h    Dup    0
dec_out            DB    010h    Dup    0
pand_1             DB    00h, 00h, 00h, 00h, 0FFh, 0FFh, 0FFh, 0FFh,  00h,  00h,  00h,  00h,  00h,  00h,  00h,  00h
pand_2             DB    00h, 00h, 00h, 00h,  00h,  00h,  00h,  00h, 0FFh, 0FFh, 0FFh, 0FFh,  00h,  00h,  00h,  00h
pand_3             DB    00h, 00h, 00h, 00h,  00h,  00h,  00h,  00h,  00h,  00h,  00h,  00h, 0FFh, 0FFh, 0FFh, 0FFh


.Code

start:
    Invoke GetModuleHandleA, 0
    Mov [h_inst], Rax
    Invoke Main
    
exit:
    Invoke CloseHandle, [h_out]
    Invoke CloseHandle, [h_in]
    Invoke CloseHandle, [h_inst]
    Invoke ExitProcess, 0
    
genr_next_block:    ; Xmm0[3] contains SXR, Xmm1 contains prev key block, Rax contains Addr enc_key
    Psrldq  Xmm0, 0Ch    ; shift SXR into Xmm0[0]
    Pxor    Xmm1, Xmm0    ; Return W_{i-4} Xor SXR (1st col of new key block) in Xmm0[0].
    Movaps  Xmm0, Xmm1
    Pslldq  Xmm0, 4
    Pand    Xmm0, [pand_1]
    Pxor    Xmm1, Xmm0    ; Return W_{i-4} Xor W{i-1} (2nd col of new key block) in Xmm0[1].
    Movaps  Xmm0, Xmm1			
    Pslldq  Xmm0, 4				
    Pand    Xmm0, [pand_2]		
    Pxor    Xmm1, Xmm0    ; 3rd col
    Movaps  Xmm0, Xmm1			
    Pslldq  Xmm0, 4			
    Pand    Xmm0, [pand_3]		
    Pxor    Xmm1, Xmm0    ; 4th col (last of block)
    Add     Rax, 010h			 
    Movaps  [Rax], Xmm1    ; store new block as next 128-bits of key expansion
    Ret
    
expand_key Frame key_ptr
    ;=======================================================================
    ; Return 0B0h byte expanded key for 128-bit AES encryption in [key_ptr].
    ;=======================================================================
    Uses Rax
    Mov Rax, [key_ptr]
    Movaps Xmm1, [Rax]
    Aeskeygenassist Xmm0, Xmm1, 01h    ; Return SubBytes Xor Rcon (SXR) in Xmm0[3].
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 02h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 04h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 08h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 010h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 020h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 040h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 080h
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 01Bh
    Call genr_next_block
    Aeskeygenassist Xmm0, Xmm1, 036h
    Call genr_next_block
    Ret
    
EndF

encrypt_16_bytes Frame plain_txt_ptr, key_ptr
    ;========================================================
    ; Return encrypted 010h byte encrypted plaintext to Xmm0.
    ;========================================================
    Uses Rax, Rcx, Rdx
    Mov Rax, [plain_txt_ptr]
    Mov Rdx, [key_ptr]
    Movaps  Xmm0, [Rax]
    Pxor Xmm0, [Rdx]
    Mov Rcx, 9 
:   Add Rdx, 010h
    Aesenc Xmm0, [Rdx]
    Loop <
    Aesenclast Xmm0, [Rdx+010h] 
    Ret
    
EndF

rdm_key Frame seed_ptr, key_ptr
    ;====================================================================================================
    ; Return a 16 byte pseudo-random key generated by a variant of the middle-square method to [key_ptr].
    ;====================================================================================================
    Uses Rax, Rcx, Rdx, Rsi
    Mov Rax, [seed_ptr]
    Mov Rcx, 0
    Mov Rdx, [key_ptr]
    Mov Rsi, [Rax]
    genr_rand:
        Cmp Rcx, 010h
        Je > rtn
        IMul Rsi, Rsi    ; square 1 byte number into a 2 byte number
        Shr Rsi, 4    ; move middle byte of result to Sil
        And Rsi, 0FFh    ; clear the rest of Rsi
        Mov B[Rdx+Rcx], Sil
        Or Rsi, 044h    ; prevent Rsi from spiraling towards 0
        Inc Rcx
        Jmp < genr_rand
    rtn:
        Ret
        
EndF

decrypt_16_bytes Frame enc_txt_ptr, key_ptr
    ;==============================================
    ; Return decrypted 010h byte plaintext to Xmm0.
    ;==============================================
    Uses Rax, Rcx, Rdx
    Mov Rax, [enc_txt_ptr]
    Mov Rdx, [key_ptr]
    Movaps  Xmm0, [Rax]
    Add Rdx, 0A0h    ; go to last enc key exp round
    Pxor Xmm0, [Rdx]
    Mov Rcx, 9 
:   Sub Rdx, 010h    ; work way backwards through key
    Aesimc Xmm1, [Rdx]    ; InvMixColumn xfm round key
    Aesdec Xmm0, Xmm1
    Loop <
    Sub Rdx, 010h
    Aesdeclast Xmm0, [Rdx] 
    Ret
    
EndF

Main Frame
    Invoke GetStdHandle, -10
    Mov [h_in], Eax
    Invoke GetStdHandle, -11
    Mov [h_out], Eax
    
menu:
    Invoke WriteFile, [h_out], Addr menu_msg, [menu_len], Addr b_out, 0
    Invoke ReadFile, [h_in], Addr char_in, 3, Addr b_in, 0    ; 3 bytes input for char+lf+cr
    Xor  Rax, Rax
    Mov  Al, [char_in]   
    check_menu_in:
        Cmp Al, 030h    ; check if inputted ASCII char is in range [030h, 035h] -> [0, 5]
        Jl > err_menu_in
        Cmp Al, 035h
        Jg > err_menu_in
        Sub Al, 030h    ; convert menu input ASCII char to hex no
        Shl  Rax, 3    ; multiply menu option by DQ jump table option offset 
        Call [jump_table+Rax]
        Jmp  < menu
        err_menu_in:
            Invoke WriteFile, [h_out], Addr menu_err_msg, [menu_err_len], Addr b_out, 0
            Jmp < menu
            
test_cpu:
    Mov Eax, 1
    CpuId	
    And Ecx, 02000000h    ; check Ecx bit 25 for AES capability
    Jz > aes_false 
    Invoke WriteFile, [h_out], Addr aes_true_msg, [aes_true_len], Addr b_out, 0
    Invoke expand_key, Addr enc_key_test
    Invoke encrypt_16_bytes, Addr plain_txt_test, Addr enc_key_test
    Movdqa Xmm1, [cipher_test]
    Comisd Xmm0, Xmm1
    Jne > cipher_false  
    Invoke WriteFile, [h_out], Addr cipher_test_true_msg, [cipher_test_true_len], Addr b_out, 0
    Jmp < menu
    aes_false:
        Invoke WriteFile, [h_out], Addr aes_false_msg, [aes_false_len], Addr b_out, 0
        Invoke ExitProcess, 1
    cipher_false:
        Invoke WriteFile, [h_out], Addr cipher_test_false_msg, [cipher_test_false_len], Addr b_out, 0
        Invoke ExitProcess, 2 
    
genr_key:
    Invoke WriteFile, [h_out], Addr gen_key_msg, [gen_key_len], Addr b_out, 0
    Invoke ReadFile, [h_in], Addr char_in, 3, Addr b_in, 0
    Xor Rax, Rax
    Mov  Al, [char_in]
    check_seed:
        Cmp Al, 041h    ; check if inputted ASCII char is in range [041h, 05Ah] -> [A, Z]
        Jl > err_seed
        Cmp Al, 05Ah
        Jg > err_seed
        Lea Rcx, Addr gen_key_seed
        Mov [Rcx], Al
        Invoke rdm_key, Addr gen_key_seed, Addr enc_key_out
        Invoke CreateFileA, Addr key_filename, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
        Mov [h_file_out], Eax
        Cmp Eax, INVALID_FILE_HANDLE
        Je > err_key_out_file
        Lea R12, Addr enc_key_out
        Mov R13, 0
        hex_to_ascii:
            Cmp R13, 010h
            Je > key_out_true
            Mov Sil, B[R12+R13]
            Mov Dil, Sil
            Shr Dil, 4    ; get 1st nibble of enc key byte
            Cmp Dil, 9
            Jle > numeric_1st_nibble
            Add Dil, 037h    ; turn hex no into ASCII char
            Mov [key_byte_out], Dil
            Invoke WriteFile, [h_file_out], Addr key_byte_out, 1, Addr b_out, 0    ; write ASCII char of 1st key byte nibble to file
            Jmp > 2nd_nibble
            numeric_1st_nibble:
                Add Dil, 30h
                Mov [key_byte_out], Dil
                Invoke WriteFile, [h_file_out], Addr key_byte_out, 1, Addr b_out, 0 
            2nd_nibble:
                And Sil, 0Fh    ; get 2nd nibble of enc key byte
                Cmp Sil, 9
                Jle > numeric_2nd_nibble
                Add Sil, 037h
                Mov [key_byte_out], Sil
                Invoke WriteFile, [h_file_out], Addr key_byte_out, 1, Addr b_out, 0    ; write ASCII char of 2nd key byte nibble to file
                Inc R13
                Jmp < hex_to_ascii
                numeric_2nd_nibble:
                    Add Sil, 030h
                    Mov [key_byte_out], Sil
                    Invoke WriteFile, [h_file_out], Addr key_byte_out, 1, Addr b_out, 0
                    Inc R13
                    Jmp < hex_to_ascii
        key_out_true:
            Invoke WriteFile, [h_out], Addr key_out_file_true_msg, [key_out_file_true_len], Addr b_out, 0
            Invoke CloseHandle, [h_file_out]
            Jmp < menu
        err_seed:
            Invoke WriteFile, [h_out], Addr menu_err_msg, [menu_err_len], Addr b_out, 0
            Call genr_key
        err_key_out_file:
            Invoke WriteFile, [h_out], Addr key_out_file_err_msg, [key_out_file_err_len], Addr b_out, 0
            Jmp < menu
            
store_key:
    Invoke CreateFileA, Addr key_filename, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    Mov [h_file_in], Eax
    Cmp Eax, INVALID_FILE_HANDLE
    Je > err_key_in_file
    Invoke GetFileSize, [h_file_in], 0
    Cmp Eax, 020h
    Jne > err_key_in_file    ; key.txt should contain a human-readable, 32-hex char key
    Mov R12, Addr enc_key_in
    Mov R13, 0
    read_key:
        Cmp R13, 010h
        Je > key_stored
        Invoke ReadFile, [h_file_in], Addr char_in, 1, Addr b_out, 0 ; read one char and turn into nibble
        Xor Rsi, Rsi
        Mov Sil, [char_in]
        Cmp Sil, 030h    ; make sure char is a hex no: [030h, 039h]+[041h, 046h] -> [1, 9]+[A, F]
        Jl > err_key_in_file
        Cmp Sil, 039h
        Jle > num_in_1st_nibble
        Cmp Sil, 041h
        Jl > err_key_in_file
        Cmp Sil, 046h
        Jg > err_key_in_file
        Sub Sil, 037h
        Mov Dil, Sil
        Shl Dil, 4    ; move hex no into 1st nibble of byte
        Jmp > in_2nd_nibble
        num_in_1st_nibble:
            Sub Sil, 030h
            Mov Dil, Sil
            Shl Dil, 4
        in_2nd_nibble:
            Invoke ReadFile, [h_file_in], Addr char_in, 1, Addr b_out, 0
            Xor Rsi, Rsi
            Mov Sil, [char_in]
            Cmp Sil, 030h
            Jl > err_key_in_file
            Cmp Sil, 039h
            Jle > num_in_2nd_nibble
            Cmp Sil, 041h
            Jl > err_key_in_file
            Cmp Sil, 046h
            Jg > err_key_in_file
            Sub Sil, 037h
            Or Sil, Dil    ; move first nibble into byte
            Mov B[R12+R13], Sil    ; move hex no byte into key addr
            Inc R13
            Jmp < read_key
            num_in_2nd_nibble:
                Sub Sil, 030h
                Or Sil, Dil    ; move first nibble into byte
                Mov B[R12+R13], Sil    ; move hex no byte into key addr
                Inc R13
                Jmp < read_key
    key_stored:
        Invoke expand_key, Addr enc_key_in
        Invoke WriteFile, [h_out], Addr key_in_file_true_msg, [key_in_file_true_len], Addr b_out, 0
        Invoke CloseHandle, [h_file_in], 0
        Jmp < menu
    err_key_in_file:
        Invoke WriteFile, [h_out], Addr key_in_file_err_msg, [key_in_file_err_len], Addr b_out, 0
        Jmp < menu
        
encrypt:
    Mov Rax, Addr enc_key_in 
    Movaps Xmm0, [Rax+010h]    ; check if the key has been expanded
    Pxor Xmm1, Xmm1
    Comisd Xmm0, Xmm1
    Je > no_enc_key_in
    Invoke CreateFileA, Addr plaintext_filename, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    Mov [h_file_in], Eax
    Cmp Eax, INVALID_FILE_HANDLE
    Je > err_plain_file_in
    Invoke GetFileSize, [h_file_in], 0
    Mov R12, Eax
    Mov R13, R12
    Shr R12, 4    ; get the floor of the number of 16-byte lines to read/write
    And R13, 0Fh    ; see if there is a remainder
    Cmp R13, 0
    Je > no_remainder
    Inc R12
    no_remainder:
        Invoke CreateFileA, Addr enc_filename, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
        Mov [h_file_out], Eax
        Cmp Eax, INVALID_FILE_HANDLE
        Je > err_enc_file_out
        encrypt_file:
            Cmp R12, 1
            Je > last_line
            Invoke ReadFile, [h_file_in], Addr plaintext_in, 010h, Addr b_out, 0
            Invoke encrypt_16_bytes, Addr plaintext_in, Addr enc_key_in
            Lea R13, Addr enc_out
            Movaps [R13], Xmm0
            Invoke WriteFile, [h_file_out], Addr enc_out, 010h, Addr b_out, 0
            Dec R12
            Jmp < encrypt_file
            last_line:
                Lea Rax, Addr plaintext_in
                Pxor Xmm2, Xmm2
                Movaps [Rax], Xmm2    ; clear the rest of the plaintext line in case the last line is < 16 bytes
                Invoke ReadFile, [h_file_in], Addr plaintext_in, 010h, Addr b_out, 0
                Invoke encrypt_16_bytes, Addr plaintext_in, Addr enc_key_in
                Lea R13, Addr enc_out
                Movaps [R13], Xmm0
                Invoke WriteFile, [h_file_out], Addr enc_out, 010h, Addr b_out, 0
                Invoke WriteFile, [h_out], Addr enc_file_true_msg, [enc_file_true_len], Addr b_out, 0
                Invoke CloseHandle, [h_file_in]
                Invoke CloseHandle, [h_file_out]
                Jmp < menu
    no_enc_key_in:
        Invoke WriteFile, [h_out], Addr key_in_enc_err_msg, [key_in_enc_err_len], Addr b_out, 0
        Jmp < menu
    err_plain_file_in:
        Invoke WriteFile, [h_out], Addr plain_file_err_msg, [plain_file_err_len], Addr b_out, 0
        Jmp < menu
    err_enc_file_out:
        Invoke WriteFile, [h_out], Addr enc_file_out_err_msg, [enc_file_out_err_len], Addr b_out, 0
        Jmp < menu
        
decrypt:
    Mov Rax, Addr enc_key_in 
    Movaps Xmm0, [Rax+010h]
    Pxor Xmm1, Xmm1
    Comisd Xmm0, Xmm1
    Je > no_dec_key_in
    Invoke CreateFileA, Addr enc_filename, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
    Mov [h_file_in], Eax
    Cmp Eax, INVALID_FILE_HANDLE
    Je > err_enc_file_in
    Invoke GetFileSize, [h_file_in], 0
    Mov R12, Eax
    Mov R13, R12
    Shr R12, 4    ; divide file size by 16, shouldn't have remainder
    Invoke CreateFileA, Addr dec_filename, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
    Mov [h_file_out], Eax
    Cmp Eax, INVALID_FILE_HANDLE
    Je > err_dec_file_out
    decrypt_file:
        Cmp R12, 0
        Je > decryption_true
        Invoke ReadFile, [h_file_in], Addr enc_in, 010h, Addr b_out, 0
        Invoke decrypt_16_bytes, Addr enc_in, Addr enc_key_in
        Lea R13, Addr dec_out
        Movaps [R13], Xmm0
        Invoke WriteFile, [h_file_out], Addr dec_out, 010h, Addr b_out, 0
        Dec R12
        Jmp < decrypt_file
        decryption_true:
            Invoke WriteFile, [h_out], Addr dec_file_true_msg, [dec_file_true_len], Addr b_out, 0
            Invoke CloseHandle, [h_file_in]
            Invoke CloseHandle, [h_file_out]
            Jmp < menu
    no_dec_key_in:
        Invoke WriteFile, [h_out], Addr key_in_enc_err_msg, [key_in_enc_err_len], Addr b_out, 0
        Jmp < menu
    err_enc_file_in:
        Invoke WriteFile, [h_out], Addr enc_file_in_err_msg, [enc_file_in_err_len], Addr b_out, 0
        Jmp < menu
    err_dec_file_out:
        Invoke WriteFile, [h_out], Addr dec_file_out_err_msg, [dec_file_out_err_len], Addr b_out, 0
        Jmp < menu
        
        Ret
    
EndF