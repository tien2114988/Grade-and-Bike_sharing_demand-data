#Chuong trinh: merge sort số thực 
#Data segment
	.data
#Cac dinh nghia bien
tenfile:	.asciiz	"FLOAT15.BIN"
float_arr : .space 60
float_tempArr : .space 60 
int_n :.word 15 
fdescr:	.word	0	
#Cac cau nhac nhap xuat du lieu
xuat_mbd :.asciiz "mang ban dau : "
xuat_mdsx :.asciiz "mang da sap xep : "
str_tc:	.asciiz	"Thanh cong."
str_loi:	.asciiz	"Mo file bi loi."
#Code segment
	.text
	.globl	main
main:	
#Doc file
  	# mo file doc
	la	$a0,tenfile
	addi	$a1,$zero,0	#flag=0:read only
	addi	$v0,$zero,13
	syscall
	bltz	$v0,baoloi
	sw	$v0,fdescr
  	# ghi file
  	lw	$a0,fdescr
   	# 4 byte so thuc
  	la	$a1,float_arr
  	addi	$a2,$zero,60
  	addi	$v0,$zero,14
  	syscall
  	# dong file
	lw	$a0,fdescr
	addi	$v0,$zero,16
	syscall	
	j	Kthucdocfile
	
	baoloi:	
	la	$a0,str_loi
	addi	$v0,$zero,4
	syscall
	
	Kthucdocfile:
#Nhap (syscall)
	addi $v0,$zero,4
	la $a0,xuat_mbd
	syscall
	
	la $a0,float_arr #a0=addr([float_arr[0])
	addi $a1,$zero,0 #a1=0=left
	lw $a3,int_n #a3=n
	addi $a2,$a3,-1 #a2=n-1=right
	jal display #xuat mang ban dau 
	
#Xu ly
	add $a3,$a1,$a2 
	srl $a3,$a3,1 #a3=pivot=(left+right)/2
	jal mergeSort #goi ham mergeSort
	
#Xuat ket qua (syscall)
	addi $t0,$a0,0 #t0=temp=a0
	addi $v0,$zero,4
	la $a0,xuat_mdsx
	syscall
	
	addi $a0,$t0,0 #a0=t0=addr([float_arr[0])
	lw $a3,int_n #a3=n
	jal display #xuat mang sau khi sap xep
	
#ket thuc chuong trinh (syscall)
	addiu	$v0,$zero,10
	syscall	

	


#--------------------------------
# Ham : xuat mang
# Input : a0=addr(arr[]), a3=n
# Output : none 
#--------------------------------
display:
	 #a0=addr(arr[0]), a3=n, s0=i(=0), t1=temp, s1=addr(arr[i])
	addi $s0,$zero,0 #i=0
	addi $s1,$a0,0
	#for(i=0;i<n;i++) cout<<arr[i];
	#fcond
	fcond1:
	beq $s0,$a3,endf1
	#fbody
	l.s $f12,0($s1)
	addi $v0,$zero,2
	syscall
	
	addi $t1,$a0,0
	addi $a0,$zero,' '
	addi $v0,$zero,11
	syscall
	addi $a0,$t1,0
	#floop
	addi $s0,$s0,1
	addi $s1,$s1,4
	j fcond1
	#endf
	endf1:
	 
	addi $t1,$a0,0
	addi $a0,$zero,'\n'
	addi $v0,$zero,11
	syscall
	addi $a0,$t1,0

	jr $ra





#--------------------------------
# Ham : merge
# Input : a0=addr(arr[0]) , a1=left , a2=right, a3=pivot 
# Output : none 
#--------------------------------
merge:
	# a0=addr(arr[0]) ,a1=left , a2=right, a3=pivot, t0=addr(tempArr[0])
	la $t0,float_tempArr 
	
	#------------- lưu mảng arr vào một mảng tạm tempArr -------------
	#s0=right-left+1(số phần tử đang merge), s1=i(=0), s2=left+i, f1=arr[left+i], t1=addr(arr[left+i]), t2=addr(tempArr[i])
	sub $s0,$a2,$a1 
	addi $s0,$s0,1 #s0=right-left+1
	addi $s1,$zero,0 # i=0
	add $s2,$a1,$s1 # s2=left+i
	sll $s2,$s2,2 
	add $t1,$a0,$s2
	add $t2,$t0,$s1
	#for(i=0,i<r-l+1,i++)
	#fcond 
	fcond2:
	beq $s1,$s0,endf2
	#fbody 
	lwc1 $f1,0($t1)
	swc1 $f1,0($t2)
	#floop 
	addi $s1,$s1,1 
	addi $t1,$t1,4
	addi $t2,$t2,4
	j fcond2 
	#endf 
	endf2:
	
	
	
	
	
	#------------- merge -------------
	#s0=i(=0), s1=j(=pivot-left+1), s2=k(=left), f1=tempArr[i], f2=tempArr[j], t1=addr(arr[k]), t2=addr(tempArr[i]), t3=addr(tempArr[j]), s3=pivot-l, s4=r-l, t4,t5
	addi $s0,$zero,0 #i=0
	sub $s3,$a3,$a1 #s3=pivot-left
	addi $s1,$s3,1 #j=pivot-left+1
	addi $s2,$a1,0 #k=left
	sub $s4,$a2,$a1 #s4=r-l
	
	sll $s0,$s0,2
	sll $s1,$s1,2
	sll $s2,$s2,2
	add $t1,$a0,$s2 #t1=addr(arr[k])
	add $t2,$t0,$s0 #t2=addr(tempArr[i])
	add $t3,$t0,$s1 #t3=addr(tempArr[j])
	srl $s0,$s0,2
	srl $s1,$s1,2
	srl $s2,$s2,2
	
	#while(i<=pivot-l && j<=r-l)
	#wcond
	wcond1:
	slt $t4,$s3,$s0 #pivot-l<i
	xori $t4,$t4,1 #i<=pivot-l
	slt $t5,$s4,$s1 #r-1<j
	xori $t5,$t5,1 #j<=r-l
	and $t4,$t4,$t5 #i<=pivot-l && j<=r-l
	beq $t4,$zero,endw1
	#wbody
	lwc1 $f1,0($t2)
	lwc1 $f2,0($t3)
	#if(tempArr[i]<=tempArr[j]){arr[k]=tempArr[i] ; i++;}
	#ifcond
	c.le.s $f1,$f2
	bc1f else1
	#ifbody
	swc1 $f1,0($t1) #arr[k]=tempArr[i]
	addi $s0,$s0,1 #i++
	addi $t2,$t2,4
	j endif1
	#else {arr[k]=tempArr[j] ; j++;}
	else1:
	#elsebody
	swc1 $f2,0($t1) #arr[k]=tempArr[j]
	addi $s1,$s1,1 #j++
	addi $t3,$t3,4
	#endif
	endif1:
	addi $s2,$s2,1 #k++
	addi $t1,$t1,4
	j wcond1
	#endw
	endw1:
	
	
	#while(i<=pivot-l)
	wcond2:
	slt $t4,$s3,$s0 #pivot-l<i
	xori $t4,$t4,1 #i<=pivot-l
	beq $t4,$zero,endw2
	#wbody
	lwc1 $f1,0($t2)
	swc1 $f1,0($t1)
	addi $s0,$s0,1 #i++
	addi $t2,$t2,4
	addi $s2,$s2,1 #k++
	addi $t1,$t1,4
	j wcond2
	#endw
	endw2:
	
	#while(j<=r-l)
	wcond3:
	slt $t5,$s4,$s1 #r-1<j
	xori $t5,$t5,1 #j<=r-l
	beq $t5,$zero,endw3
	#wbody
	lwc1 $f2,0($t3)
	swc1 $f2,0($t1)
	addi $s1,$s1,1 #j++
	addi $t3,$t3,4
	addi $s2,$s2,1 #k++
	addi $t1,$t1,4
	j wcond3
	#endw
	endw3:
	
	jr $ra
				
					
						
	
				
#--------------------------------
# Ham : mergeSort
# Input : a0=addr(arr[0]), a1=left,a2=right
# Output : none 
# reserved : ra,a1,a2,a3
#--------------------------------
mergeSort:
	 #a0=addr(arr[0]), a1=left, a2=right, a3=pivot(=(right+left)/2), $t6=($a1<$a2)
	 addi $sp,$sp,-16
	 sw $ra,0($sp)
	 sw $a1,4($sp)
	 sw $a2,8($sp)
	 
	 add $a3,$a1,$a2
	 srl $a3,$a3,1
	 sw $a3,12($sp)
	 #if(left<right)
	 #ifcond
	 slt $t6,$a1,$a2
	 beq $t6,$zero,endif2
	 #ifbody
	 add $a2,$a3,$zero #right=pivot
	 jal mergeSort
	 
	 lw $a1,4($sp)
	 lw $a2,8($sp)
	 lw $a3,12($sp)
	 addi $a1,$a3,1 #left=pivot+1
	 jal mergeSort
	 
	 lw $a1,4($sp)
	 lw $a2,8($sp)
	 lw $a3,12($sp)
	 jal merge
	 #endif
	 endif2:
	 
	 
	 lw $a3,int_n 
	 jal display #xuat ket qua tai tung buoc
	 
	 lw $a3,12($sp)
	 lw $ra,0($sp)
	 addi $sp,$sp,16
	 
	 jr $ra
