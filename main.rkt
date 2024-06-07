;yusuf anil yazici
;2021400207
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

; 3.1 converts a binary string to its decimal equivalent
(define (binary_to_decimal binary)
   ; creates a loop with bits and result variables
  (let loop ((bits (string->list binary)) (result 0))
    ; checks if bits is empty, returns result if true
    (if (null? bits)
        result
        ; updates result by adding the value of the current bit
        (loop (cdr bits) (+ (* result 2) (if (char=? (car bits) #\1) 1 0))))))

; 3.2 relocates addresses based on limit and base
(define (relocator args limit base)
  ; maps each binary address to a physical address
  (map
    (lambda (binary-address)
      ; converts binary address to decimal
      (let ((decimal-address (binary_to_decimal binary-address)))
        ; calculates physical address
        (let ((physical-address (+ decimal-address base)))
          ; checks if decimal address exceeds limit, returns -1 if true
          (if (> decimal-address limit)                    
              -1                                            
              physical-address))))                        
    args))

; 3.3 divides address space into page number and offset
(define (divide_address_space num page_size)
  ; defines page size bits and page number bits
  (let* ((page_size_bits (+ 10 (inexact->exact (floor (/ (log page_size) (log 2))))))  
         (page_number_bits (max 0 (- (string-length num) page_size_bits)))          
         (page_number (substring num 0 page_number_bits))                           
         (page_offset (substring num page_number_bits)))                           
    (list page_number page_offset)))                                                
                                                           
; 3.4 maps virtual addresses to physical addresses using page table
(define (page args page_table page_size)
  ; defines page size bits
  (let* ((page_size_bits (+ 10 (floor (/ (log page_size) (log 2))))))
    ; maps each argument to a physical address
    (map
      (lambda (arg)
        ; splits address into page number and offset
        (let* ((split_addr (divide_address_space arg page_size))
               (page_number (car split_addr))                         
               (page_offset (cadr split_addr))
                ; converts page number to decimal
               (page_number_decimal (string->number page_number 2))
               ; retrieves frame number from page table
               (frame_number (if (< page_number_decimal (length page_table))
                                (list-ref page_table page_number_decimal)
                                "ERROR")))
          ; returns frame number with offset or error
          (if (string=? frame_number "ERROR")
              frame_number
              (string-append frame_number page_offset))))             
      args)))


; calculates the factorial of a number
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

; calculates a term in the taylor series for sine
(define (taylor-term x n)
  (/ (expt x (+ (* 2 n) 1))
     (factorial (+ (* 2 n) 1))))

; 3.5 calculates the sine of x using taylor series
(define (find_sin x num)
  (let* ((radians (/ (* x pi) 180))
         (terms (map (lambda (n)
                       (* (expt -1 n) (taylor-term radians n)))
                     (range 0 num))))
    (apply + terms)))


; sums the first ten digits of the decimal part of a number
(define (sum-first-ten-digits num)
  (let* ((str (number->string num))
         (decimal-part (second (regexp-split #px"\\." str)))
         (digits (substring decimal-part 0 (min 10 (string-length decimal-part)))))
    (apply + (map (lambda (c) (- (char->integer c) 48)) (string->list digits)))))

; 3.6 calculates a hash value for a binary address
(define (myhash arg table-size)
  (let* ((decimal-value (binary_to_decimal arg))
         (n (+ (modulo decimal-value 5) 1))
         (sin-value (find_sin decimal-value n))
         (sum-digits (sum-first-ten-digits sin-value)))
    (modulo sum-digits table-size)))

; 3.7 maps virtual address to physical address using hashed page table
(define (hashed_page arg table-size page-table page-size)
    ; splits address into page number and offset
  (let* ((split-addr (divide_address_space arg page-size))
         (page-number (car split-addr))
         (page-offset (cadr split-addr))
         (page-number-decimal (string->number page-number 2))
         ; calculates hash value for page number
         (hash-value (myhash page-number table-size))
         (bucket (list-ref page-table hash-value)))
    ; searches for page number in bucket
    (let loop ((bucket bucket))
      (if (null? bucket)
          "ERROR"
          (let ((entry (car bucket)))
            (if (string=? (car entry) page-number)
                (string-append (cadr entry) page-offset)
                (loop (cdr bucket))))))))

; 3.8 splits addresses into chunks of specified size
(define (split_addresses args size)
  (let loop ((addresses (string->list args)) (result '()))
    (if (null? addresses)
        (reverse result)
        (let ((chunk (take addresses size)))
          (loop (drop addresses size) (cons (list->string chunk) result))))))

; 3.9 maps addresses using hashed page table
(define (map_addresses args table-size page-table page-size address-space-size)
  ; splits arguments into chunks of address space size
  (let ((split-args (split_addresses args address-space-size)))
    ; maps each argument to a physical address
    (map (lambda (arg)
           (hashed_page arg table-size page-table page-size))
         split-args)))
