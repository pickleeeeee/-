assume cs:code,ds:data,es:table_
;主栈区，存放传递的参数
stack_ segment
    db 160 dup (0)
stack_ ends
;ds
data_1 segment
    
    ;设置di=0h
    ;偏移地址di开始
    ;年份信息，一个年份占四个字节，di指向年份
    ;4*21=84字节
    db '1975','1976','1977','1978','1979','1980','1981','1982','1983'
    db '1984','1985','1986','1987','1988','1989','1990','1991','1992'
    db '1993','1994','1995'
    ;偏移地址di+84
    ;收入信息，占四个字节，di同时作为收入信息下标的一部分
    dd 16, 22, 382, 1356, 2390, 8000, 16000, 24486, 50065, 97479, 140417, 197514
    dd 345980, 590287, 803530, 1183000, 1843000, 2759000, 3753000, 4649000, 5937000
    ;偏移地址si+168
    ;员工数量信息，占两个字节，si作为下边
    dw 3, 7, 9, 13, 28, 38, 130, 220, 476, 778, 1001, 1442, 2258, 2793, 4037, 5635, 8226
    dw 11542, 14430, 15257, 17800
data_1 ends
;存储最终结果
;es:bp
table_ segment
    db 21 dup ('year summ ne ?? ')
table_ ends
;主函数main
code segment
start:
    mov ax,stack_        ;初始化大栈区
    mov ss,ax
    mov sp,160
    ;初始化ds
    mov ax,data_1
    mov ds,ax
    ;初始化di
    mov di,0h
    ;初始化si
    mov si,0h
    ;初始化cx,一共21行，循环21次
    mov cx,21
    ;初始化es：bp指向table
    mov ax,table_
    mov es,ax
    mov bp,0h
    ;循环
  s:
    push cx
    ;年份信息,一次搬运16bit
    mov ax,ds:[di]
    mov es:[bp],ax
    mov ax,ds:[di+2]
    mov es:[bp+2],ax
    mov ax,bp       ;被除数
    mov cl,16
    div cl
    mov ah,al   ;行
    mov al,0    ;列
    push ax     ;行列
    push bp     ;下标
    mov ax,table_
    push ax       ;段地址
    call show_str
    ;收入信息
    mov ax,ds:[di+84]
    mov es:[bp+5],ax
    mov ax,ds:[di+84+2]
    mov es:[bp+7],ax
    push ds:[di+84]
    push ds:[di+84+2]
    call dtoc
    mov ax,bp       ;被除数
    mov cl,16
    div cl
    mov ah,al   ;行
    mov al,6    ;列
    push ax     ;行列
    mov ax,0
    push ax     ;下标
    mov ax,data
    push ax     ;段地址
    call show_str
    ;雇员信息
    mov ax,ds:[si+168]
    mov es:[bp+10],ax   
    push ds:[si+168]
    mov ax,0
    push ax
    call dtoc
    mov ax,bp       ;被除数
    mov cl,16
    div cl
    mov ah,al   ;行
    mov al,17    ;列
    push ax     ;行列
    mov ax,0
    push ax     ;下标
    mov ax,data
    push ax     ;段地址
    call show_str
    ;人均收入
    mov ax,es:[bp+5]
    mov dx,es:[bp+7]
    div word ptr es:[bp+10]    ;这里的div操作并不会溢出
    
    mov es:[bp+13],ax
    mov byte ptr es:[bp+0fh],20h
    push es:[bp+13]
    mov ax,0
    push ax
    call dtoc
    mov ax,bp       ;被除数
    mov cl,16
    div cl
    mov ah,al   ;行
    mov al,23    ;列
    push ax     ;行列
    mov ax,0        ;下标
    push ax
    mov ax,data        ;段地址
    push ax
    call show_str
    ;移动索引
    add di,4
    add si,2
    add bp,16
    pop cx
    sub cx,1
    jcxz o
    
    jmp far ptr s
  o:mov ax,4c00h
    int 21h
;------------------------------------------------------------------------------------------------------------------------------------
;打印一个32bit数字的功能
;用到的寄存器 ss sp ax es di si dx
;定义一个栈区用来反转字符串,私有栈
stack_d segment
    db 32 dup (0)
stack_d ends
;结果存放区
data segment
    db 10 dup (0)
data ends
dtoc :
    push cx
    push ax
    push es
    push di
    push si
    push dx
    push bp
    mov bp,sp       ;存放主栈区当前的下标
    mov dx,ss:[bp+16]        ;存放高16位
    mov ax,ss:[bp+18]        ;存放低16位
    mov si,10                ;存放被除数
    ;初始化小栈区
    mov di,stack_d
    mov ss,di
    mov sp,32       ;指向栈底
   
    ;存放转换结果的段地址
    mov di,data     
    mov es,di
    mov di,0
  s1:
    call divdw      ;cx余数小于10
    add cx,30h      ;转化为ASCII码
    push cx         ;入栈+2
    ;判断是否取余结束了
    mov cx,dx
    or cx,ax
    jcxz ok
    jmp s1
 ok:
    mov cx,32
    sub cx,sp    ;cx保存了数字长度
    mov ax,cx
    mov dl,2
    div dl
    mov cl,al
    mov ch,0
  s2:pop es:[di]
    add di,1    ;只有低八位有数字所以为了紧凑只+1
    loop s2
    mov byte ptr es:[di],20h ;添加结束空格符号
    
    ;切换回主栈区
    mov ax,stack_
    mov ss,ax
    mov sp,bp
    pop bp
    pop dx
    pop si
    pop di
    pop es
    pop ax
    pop cx
    ret 4
    
;dtoc的私有函数
;进行不会产生溢出的除法运算
;目标
;(ax)=dword型数据低16位     返回值
;(dx)=dword型数据高16位     返回值
;(cx)=余数                  返回值
;(si)存放被除数             
;X/N=int(H/N)*65536+[rem(H/N)*65536+L]/N
divdw :
    mov bx,ax   ;暂存低16位
    ;计算H/N
    mov ax,dx   ;32位/16位 -> dx存放余数 ax存放结果
    mov dx,0
    div si      ;此时ax=int(H/N),dx=rem(H/N)
    
    mov di,ax   ;保存di = int(H/N)
    ;计算[rem(H/N)*65536+L]
    mov ax,bx   ;恢复原始的低16位数，此时dx已经放的是rem(H/N)了 32位/16位 -> dx存放余数 ax存放结果
    div si      ;/N
    mov cx,dx    ;cx保存余数
    mov dx,di    ;相当于di*65536->int(H/N)*65536
    
    ;运算结束
    ret
;------------------------------------------------------------------------------------------------------------------------------------
;数据放在data中 dx 行列
;显示区域 B8000H~BFFFFH
;用到的寄存器 ax cx dx si di es ds
;ax 中间替换操作
;cx 循环计数
;dx 存放行列
;si 打印数据下标
;di 目标打印区域下标
;es 目标打印区域段地址
;ds 打印数据的段地址
;遇到空格（20h）就停止打印
show_str :  
           push ax
           push cx
           push dx
           push si
           push di
           push es
           push ds
           push bp
           
           mov bp,sp
           mov ax,ss:[bp+18]    ;段地址
           mov ds,ax
           
           mov si,ss:[bp+20]    ;偏移
           mov dx,ss:[bp+22]    ;行列
           mov ax,0B000h   ;es指向显示区域的段地址
           mov es,ax
           mov di,8000h     ;di作为显示区域的下标索引
        ; 测试区域
        ;    mov ax,0800h   ;es指向显示区域的段地址
        ;    mov es,ax
        ;    mov di,0h     ;di作为显示区域的下标索引
           mov al,160        ;计算行
           mul dh
           add di,ax        ;+行偏移量
           
           mov al,2         ;计算列
           mul dl            ;ax中存放列的偏移量量
           add di,ax       ;+列偏移量
        s_3: mov cl,[si]      
           mov ch,0
           mov al,[si]     ;存放当前的字符
           mov ah,0
           sub cx,20h      ;判断是否是空格
           jcxz ok_2
           mov es:[di],al
           mov byte ptr es:[di+1],2
           add si,1
           add di,2
           loop s_3
        ok_2:
            pop bp
            pop ds
            pop es
            pop di
            pop si
            pop dx
            pop cx
            pop ax
            ret 6
;------------------------------------------------------------------------------------------------------------------------------------
code ends
end start
