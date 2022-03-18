;=======================================================================================================
; challenge_coding_in_assembler.asm
;=======================================================================================================

;=======================================================================================================
; Return my solution to the Challenge from the chapter "Coding in Assembly: I" from the course "Learning 
; Assembly Language" on LinkedIn Learning. This code is the first step towards building a file encryptor 
; that operates similarly to the Enigma machine, using hexadecimal numbers instead of the alphabet. This 
; code configures the machine by loading three analogous slots with hexadecimal rotors. Upon running the 
; code, a menu promt queries the user for one of two jump-table options. "Configure" queries the user for
; one of five rotors to choose to load into each slot and rotates them to the desired start character.
; The configuration is then printed to a human-readable file config.txt. The user can exit the code via 
; the "Exit" jump-table option.   
;=========================================================================================================

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

b_in       DB    020h    Dup    0
b_out      DB    020h    Dup    0
char_in    DB    020h    Dup    0

hex_no        DQ    0
hex_len       DQ    0
ascii_char    DB    010h    Dup    0

menu_msg    DB    'Enigma-like file encryptor', 0Dh, 0Ah
            DB    '    0...Exit', 0Dh, 0Ah
            DB    '    1...Configure', 0Dh, 0Ah
            DB    '> '
menu_len    DQ    $-menu_msg

jump_table    DQ    Addr exit
              DQ    Addr config

rot_no_msg        DB    'Please enter a unique rotor number (1-5) for slot x:', 0Ah, 0Dh, '> '
rot_no_len        DQ    $-rot_no_msg
start_char_msg    DB    'Please enter the hexadecimal start character (0-F) for slot x:', 0Ah, 0Dh, '> '
start_char_len    DQ    $-start_char_msg
in_err_msg        DB    'ERROR: Input number not in range, please try again', 0Ah, 0Dh
in_err_len        DQ    $-in_err_msg
config_file       DB    'config.txt', 0
file_err_msg      DB    'ERROR: Could not create file config.txt', 0Ah, 0Dh
file_err_len      DQ    $-file_err_msg


msg_1        DB    'Slot x:', 0Ah, 0Dh
msg_1_len    DQ    $-msg_1
msg_2        DB    '    Rotor hex: xxxxxxxxxxxxxxxx', 0Ah, 0Dh
msg_2_len    DQ    $-msg_2
msg_3        DB    '    Rotor notch: x', 0Ah, 0Dh
msg_3_len    DQ    $-msg_3
msg_4        DB    '    Rotor number: x', 0Ah, 0Dh
msg_4_len    DQ    $-msg_4
msg_5        DB    '    Start character: x', 0Ah, 0Dh
msg_5_len    DQ    $-msg_5


rotor    Struct
    hex      DQ
    notch    DB
    
EndS
rotor_1        rotor    <01F46C8037B9AD25Eh, 0Fh>
rotor_2        rotor    <0EFA87B439D5216C0h, 03h>
rotor_3        rotor    <00F732D168C4BA59Eh, 0Dh>
rotor_4        rotor    <0F0E8143CA2695B9Dh, 00h>
rotor_5        rotor    <0AB8736E1F0C295D4h, 03h>
rotor_len      DQ       $-rotor_5
chosen_rots    DQ       0

slot    Struct
    rot_ty       rotor
    rot_no       DB
    rot_start    DB
    
EndS
slot_1       slot
slot_2       slot
slot_3       slot
slot_len     DQ      $-slot_3
curr_slot    DQ      1

.Code

start:
    Invoke GetModuleHandleA, 0
    Mov [h_inst], Rax
    Invoke Main
    
exit:
    Invoke CloseHandle, [h_in]
    Invoke CloseHandle, [h_out]
    Invoke CloseHandle, [h_inst]
    Invoke ExitProcess, 0
    
load_slot Frame slot_no_ptr
    ;===============================================================================
    ; Stores the desired rotor with its desired start character in the desired slot. 
    ;===============================================================================
    Uses Rax, Rcx, Rdx, Rsi 
    
get_rot_no:
    Mov Rcx, [slot_no_ptr]
    Mov Rax, [Rcx]
    Add Rax, 030h    ; transform hex slot no (1-3) into ASCII char
    Lea Rcx, Addr rot_no_len
    Mov B[Rcx-6], Al    ; insert slot no into message
    Invoke WriteFile, [h_out], Addr rot_no_msg, [rot_no_len], Addr b_out, 0
    Invoke ReadFile, [h_in], Addr char_in, 3, Addr b_in, 0    ; 3 bytes input for char+lf+cr
    Xor Rax, Rax
    Mov Al, [char_in]
    
check_rot_no: ; Rax contains rotor no
    Cmp Al, 031h    ; check if inputted ASCII char is in range [031h, 035h] -> [1, 5]
    Jl > err_rot_no
    Cmp Al, 035h
    Jg > err_rot_no
    Sub Al, 030h    ; convert rot no ASCII char to hex no
    Mov Rcx, 1
    Mov Rdx, [chosen_rots]
    check_unique:
        Cmp Rcx, 2
        Jg > store_rot_no
        Cmp Dl, Al
        Je > err_rot_no
        Shr Rdx, 8    ; move the next previously chosen rotor no to Al
        Inc Rcx
        Jmp < check_unique
    err_rot_no:
        Invoke WriteFile, [h_out], Addr in_err_msg, [in_err_len], Addr b_out, 0
        Jmp < get_rot_no
        
store_rot_no:    ; Rax contains rotor no
    Lea Rcx, Addr slot_1
    Mov Rdx, [curr_slot]
    Dec Rdx
    IMul Rdx, [slot_len] 
    Add Rcx, Rdx    ; load effective address of current slot to Rcx
    Lea Rdx, Addr rotor_1
    Mov Rsi, Rax
    Dec Rsi
    IMul Rsi, [rotor_len] 
    Add Rdx, Rsi    ; load effective address of desired rotor to Rdx
    Mov Rsi, [Rdx]
    Mov [Rcx], Rsi    ; load the desired rotor in the current slot
    Mov Rsi, [Rdx+8]
    Mov [Rcx+8], Rsi    ; load rotor_x.notch into slot_x.rot_ty.notch
    Add Rcx, [rotor_len]
    Mov B[Rcx], Al    ; store hex rot no in slot_x.rot_no
    Lea Rcx, Addr chosen_rots
    Add Rcx, [curr_slot]
    Dec Rcx
    Mov B[Rcx], Al    ; store chosen rotor no in chosen_rots
    
get_start_char:
    Mov Rax, [curr_slot]
    Add Rax, 030h
    Lea Rcx, Addr start_char_len
    Mov B[Rcx-6], Al
    Invoke WriteFile, [h_out], Addr start_char_msg, [start_char_len], Addr b_out, 0
    Invoke ReadFile, [h_in], Addr char_in, 3, Addr b_in, 0
    Xor Rax, Rax
    Mov Al, [char_in]
    
check_start_char:    ; Rax contains start char
    Lea Rcx, Addr slot_1
    Mov Rdx, [curr_slot]
    Dec Rdx
    IMul Rdx, [slot_len] 
    Add Rcx, Rdx    ; load effective address of current slot to Rcx
    Mov Rdx, [Rcx]    ; move rotor in current slot to Rdx
    Cmp Al, 030h
    Jl > err_start_char
    Cmp Al, 039h
    Jg > start_char_g_039h
    Sub Al, 030h
    Jmp > set_start_char
    start_char_g_039h:    ; check if inputted ASCII char is in range [041h, 046h] -> [A, F]
        Cmp Al, 041h
        Jl > err_start_char
        Cmp Al, 046h
        Jg > err_start_char
        Sub Al, 37h    ; convert ASCII char to hex no
    set_start_char:
        Mov Rsi, Rdx    ; copy shifted rotor to Rsi
        And Rsi, 0Fh    ; clear Rsi except for last digit
        Cmp Al, Sil
        Je > store_start_char
        Rol Rdx, 4    ; rotate the rotor to the next char
        Jmp < set_start_char
    err_start_char:
        Invoke WriteFile, [h_out], Addr in_err_msg, [in_err_len], Addr b_out, 0
        Jmp < get_start_char
        
store_start_char:    ; Rax contains start char, Rcx contains Addr curr slot, Rdx contains corrected rotor
    Ror Rdx, 4    ; restore rotor start char to init pos
    Mov [Rcx], Rdx
    Add Rcx, [rotor_len]
    Inc Rcx
    Mov B[Rcx], Al    ; store hex rot no in slot_x.rot_start
    Ret
    
EndF

hex_to_ascii Frame hex_ptr, hex_len_ptr, target_ptr
    ;==============================================
    ; Return ASCII chars for hex no to target Addr.
    ;==============================================
    Uses Rax, Rcx, Rdx, Rsi, Rdi, R8, R9
    Mov Rax, [hex_ptr]
    Mov Rcx, [Rax]    ; move hex no onto Rcx
    Mov Rdx, [target_ptr]
    Mov Rsi, [hex_len_ptr]
    Mov Rdi, [Rsi]    ; move hex len onto Rdi
    Mov R8, 0
    hex_loop:
        Cmp R8, Rdi
        Je > rtn_ascii
        Cmp Rdi, 1
        Je > to_ascii
        Rol Rcx, 4    ; shift next hex no onto last nibble
        to_ascii:
            Mov R9, Rcx    ; copy hex no onto R9
            And R9, 0Fh    ; clear all but last nibble
            Cmp R9, 09h
            Jg > alphabetical
            Add R9, 030h
            Mov B[Rdx+R8], R9b
            Inc R8
            Jmp < hex_loop
            alphabetical:
                Add R9, 037h
                Mov B[Rdx+R8], R9b
                Inc R8
                Jmp < hex_loop
    rtn_ascii:
        Ret
    
EndF

write_slot_to_file Frame slot_no_ptr
    ;======================================================
    ; Write the current slot information to file config.txt
    ;======================================================
    Uses Rax, Rcx, Rsi, Rdi
    Mov Rdi, [slot_no_ptr]
    Mov Rsi, [Rdi]
    Add Rsi, 030h
    Lea Rdi, msg_1_len
    Mov B[Rdi-4], Sil    ; insert slot no into file msg
    Invoke WriteFile, [h_file_out], Addr msg_1, [msg_1_len], Addr b_out, 0
    Sub Rsi, 031h
    Lea Rdi, Addr slot_1
    IMul Rsi, [slot_len] 
    Add Rdi, Rsi    ; load effective address of current slot to Rdi
    Mov Rax, [Rdi]    ; move rotor hex onto Rax
    Lea Rcx, Addr hex_no
    Mov [Rcx], Rax
    Lea Rax, Addr hex_len
    Mov B[Rax], 010h
    Invoke hex_to_ascii, Addr hex_no, Addr hex_len, Addr ascii_char
    Lea Rax, Addr ascii_char
    Mov Rcx, [Rax]
    Lea Rdx, Addr msg_2_len
    Mov [Rdx-012h], Rcx    ; move first half of rotor hex into msg
    Mov Rcx, [Rax+8]
    Mov [Rdx-0Ah], Rcx    ; move second half of rotor hex into msg
    Invoke WriteFile, [h_file_out], Addr msg_2, [msg_2_len], Addr b_out, 0    
    Add Rdi, 8    ; load effective address of slot_x.rot_ty.notch to Rdi
    Mov Al, B[Rdi]
    Lea Rcx, Addr hex_no
    Mov [Rcx], Al
    Lea Rax, Addr hex_len
    Mov B[Rax], 1
    Invoke hex_to_ascii, Addr hex_no, Addr hex_len, Addr ascii_char
    Lea Rax, Addr ascii_char
    Mov Cl, B[Rax]
    Lea Rdx, Addr msg_3_len
    Mov B[Rdx-3], Cl    ; move rotor notch into message
    Invoke WriteFile, [h_file_out], Addr msg_3, [msg_3_len], Addr b_out, 0   
    Add Rdi, 8    ; load effective address of slot_x.rot_no to Rdi
    Mov Al, B[Rdi]
    Lea Rcx, Addr hex_no
    Mov B[Rcx], Al
    Lea Rax, Addr hex_len
    Mov B[Rax], 1
    Invoke hex_to_ascii, Addr hex_no, Addr hex_len, Addr ascii_char
    Lea Rax, Addr ascii_char
    Mov Cl, B[Rax]
    Lea Rdx, Addr msg_4_len
    Mov B[Rdx-3], Cl    ; move rotor notch into message
    Invoke WriteFile, [h_file_out], Addr msg_4, [msg_4_len], Addr b_out, 0   
    Inc Rdi    ; load effective address of slot_x.rot_start to Rdi
    Mov Al, B[Rdi]
    Lea Rcx, Addr hex_no
    Mov B[Rcx], Al
    Lea Rax, Addr hex_len
    Mov B[Rax], 1
    Invoke hex_to_ascii, Addr hex_no, Addr hex_len, Addr ascii_char
    Lea Rax, Addr ascii_char
    Mov Cl, B[Rax]
    Lea Rdx, Addr msg_5_len
    Mov B[Rdx-3], Cl    ; move rotor start char into message
    Invoke WriteFile, [h_file_out], Addr msg_5, [msg_5_len], Addr b_out, 0   
    Ret
EndF
	
Main Frame
    Invoke GetStdHandle, -10
    Mov [h_in], Eax
    Invoke GetStdHandle, -11
    Mov [h_out], Eax
    
get_menu_in:
    Invoke WriteFile, [h_out], Addr menu_msg, [menu_len], Addr b_out, 0
    Invoke ReadFile, [h_in], Addr char_in, 3, Addr b_in, 0
    Xor Rax, Rax
    Mov  Al, [char_in]
    
check_menu_in:    ; menu input stored in Rax
    Cmp Al, 030h    ; check if inputted ASCII char is in range [030h, 031h] -> [0, 1]
    Jl > err_menu_in
    Cmp Al, 031h
    Jg > err_menu_in
    Sub Al, 030h    ; convert menu input ASCII char to hex no
    Shl  Rax, 3    ; multiply menu option by DQ jump table option offset 
    Call [jump_table+Rax]
    Jmp  < get_menu_in
    err_menu_in:
        Invoke WriteFile, [h_out], Addr in_err_msg, [in_err_len], Addr b_out, 0
        Jmp < get_menu_in
        
config:
    Invoke CreateFileA, Addr config_file, GENERIC_WRITE, 0, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
    Mov [h_file_out], Eax
    Cmp Eax, INVALID_FILE_HANDLE
    Je > file_err
    load_slots:
        Mov Rax, [curr_slot]
        Cmp Rax, 3
        Jg > rtn_config
        Invoke load_slot, Addr curr_slot
        Invoke write_slot_to_file, Addr curr_slot
        Inc Q[curr_slot]
        Jmp < load_slots
    rtn_config:
        Invoke CloseHandle, [h_file_out]
        Ret
    file_err:
        Invoke WriteFile, [h_out], Addr file_err_msg, [file_err_len], Addr b_out, 0
        Ret
    
EndF
