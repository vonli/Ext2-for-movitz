(require :x86-pc/ata)

(provide :lib/ext2)

(defpackage #:muerte.ext2
  (:documentation "Implententation of the EXT2 filesystem.")
  (:nicknames #:ext2)
  (:use #:common-lisp )
  (:export
   #:read
   #:write))

(in-package #:muerte.ext2)

;;utilities
(defun sublist(from to l)
  (nthcdr from (nbutlast (copy-list l) (- (length l) to))))

(defun byte-list-to-number (byte-list)
  (loop
     for byte in byte-list
       for i from 0 to 511 sum
       (ash byte (* 8 (if (< 0 i)
			  (expt 2 i)
			  0)))))

(defun controller-number-to-constant (n)
  (cond
    ((= 1 n) muerte.x86-pc.ata::+controller1+)
    ((= 0 n) muerte.x86-pc.ata::+controller0+)
    (T (error "Must be 1 or 0"))))

;;

(defclass ext2 ()
  ((controller :initarg :controller :accessor controller)
   (drive-number :initarg :drive-number :accessor drive-number)
   (partition :initarg :partition :accessor partition)
   (start-offset :accessor start-offset)
   (superblock :accessor superblock)
   (block-size :accessor block-size)
   ))

(defmethod initialize-instance :after ((ext2 ext2) &rest args)
  "initializes the ext2 class. The order of setf's is important, due to the
inconsistent controller numbering in the ata driver"
  (setf
   (start-offset ext2) (muerte.x86-pc.ata::partition-start-offset
			     (muerte.x86-pc.ata::get-partition-data (controller ext2)
						 (drive-number ext2)
						 (partition ext2)))
   (controller ext2) (controller-number-to-constant (controller ext2))
   (superblock ext2) (append
			   (read-drive-block ext2 3)
			   (read-drive-block ext2 4))
   (block-size ext2) (get-block-size (superblock ext2))))

(defmethod read-drive-block ((ext2 ext2) block-address)
   (muerte.x86-pc.ata::lba-read-sector (controller ext2)
				       (drive-number ext2)
				       (+ (start-offset ext2) block-address)))

(defmethod read-block ((ext2 ext2) block-number)
  (loop for i from 1 to (/ (block-size ext2) 512) append
       (read-drive-block ext2 (+ i
				 (* block-number (/ (block-size ext2) 512))))))

;;superblock parsing

(defun get-number-of-inodes (superblock)
  (byte-list-to-number (sublist 0 3 superblock)))

(defun get-number-of-blocks (superblock)
  (byte-list-to-number (sublist 4 7 superblock)))

(defun get-start-block (superblock)
  (byte-list-to-number (sublist 20 23 superblock)))

(defun get-block-size (superblock)
  (ash 1024 (byte-list-to-number (sublist 24 27 superblock))))

(defun get-blocks-per-group (superblock)
  (byte-list-to-number (sublist 32 35 superblock)))

(defun get-inodes-per-group (superblock)
  (byte-list-to-number (sublist 40 43 superblock)))

(defun get-creator-os (superblock)
  (byte-list-to-number (sublist 72 75 superblock)))

;; Testing and convenience functions

(defun make-ext2( controller drive partition)
  (make-instance 'ext2 :controller 0 :drive-number 0 :partition 0))

(defun get-block-table (ext2)
  (read-block ext2 2))

(defun get-inode-table-address (block-table)
  (byte-list-to-number (sublist 8 11 block-table)))

(defun get-root-inode (ext2 block-table)
  (read-block ext2 (get-inode-table-address block-table)))

(defun get-type-of-root (ext2)
  (byte-list-to-number (sublist 128 130
				(get-root-inode
				 ext2
				 (get-block-table ext2)))))

