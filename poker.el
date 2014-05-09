;;; poker.el --- Poker engine for Emacs

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A collection of poker related functions.

;;; Code:

(require 'cl-lib)
(require 'cookie1)

(defconst poker-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defconst poker-suits '(clubs diamonds hearts spades))

(defsubst poker-make-card (rank suit)
  (cl-assert (memq rank poker-ranks))
  (cl-assert (memq suit poker-suits))
  (cons rank suit))

(defsubst poker-card-rank (card)
  (cl-check-type card cons)
  (car card))

(defsubst poker-card-suit (card)
  (cl-check-type card cons)
  (cdr card))

(defun poker-rank-value (rank)
  (cl-position rank poker-ranks))

(defun poker-hand-value (hand)
  "Calculate the value of a given 5 card poker HAND.
The result is a list of integers where the first (0-8) indicates the type
of hand, and the rest are rank values of decisive cards.
The highest possible value is therefore (8 12) and the lowest is (0 5 3 2 1 0)."
  (let* ((ranks (mapcar #'poker-card-rank hand))
	 (rank-counts
	  (sort (mapcar (lambda (rank)
			  (cons (cl-count rank ranks) (poker-rank-value rank)))
			(cl-remove-duplicates ranks))
		(lambda (lhs rhs) (or (> (car lhs) (car rhs))
				      (and (= (car lhs) (car rhs))
					   (> (cdr lhs) (cdr rhs))))))))
    (setq ranks (mapcar #'cdr rank-counts))
    (apply #'list
	   (pcase (mapcar #'car rank-counts)
	     (`(1 1 1 1 1)
	      (when (and (eq (nth 0 ranks) 12) (eq (nth 1 ranks) 3))
		(setq ranks '(3 2 1 0 -1)))
	      (let ((straight (eq (- (nth 0 ranks) (nth 4 ranks)) 4))
		    (flush (eq (length (cl-delete-duplicates
					(mapcar #'poker-card-suit hand))) 1)))
		(when straight (setq ranks (list (car ranks)))) ; prune
		(cond ((and straight flush) 8) (flush 5) (straight 4) (t 0))))
	     (`(2 1 1 1) 1) (`(2 2 1) 2) (`(3 1 1) 3) (`(3 2) 6) (`(4 1) 7))
	   ranks)))

(defun poker-hand-> (hand1 hand2)
  (not (version-list-<= (poker-hand-value hand1) (poker-hand-value hand2))))

(defun poker-sort-hands (hands)
  (sort hands #'poker-hand->))

(defun poker-possible-hands (cards)
  (if (<= (length cards) 5)
      (list cards)
    (apply #'nconc (mapcar (lambda (card)
			     (poker-possible-hands (cl-remove card cards)))
			   cards))))

(defun poker-best-hand (cards)
  "Find the best hand for a number of CARDS (usually 6 or 7)."
  (car (poker-sort-hands (poker-possible-hands cards))))

(defun poker-rank-value-name (rank-value)
  (let ((rank-value-names ["2" "3" "4" "5" "6" "7" "8" "9" "10"
			   "Jack" "Queen" "King" "Ace"]))
    (aref rank-value-names rank-value)))

(defun poker-rank-value-name-plural (rank-value)
  (concat (poker-rank-value-name rank-value) "s"))

(defun poker-describe-hand (hand)
  (cl-assert (eq (length hand) 5))
  (pcase (poker-hand-value hand)
    (`(8 ,high) (pcase high
		  (12 "Royal flush")
		  (_ (format "%s high straight flush"
			     (poker-rank-value-name high)))))
    (`(7 ,rank ,high) (format "Four %s, %s high"
			      (poker-rank-value-name-plural rank)
			      (poker-rank-value-name high)))
    (`(6 ,three ,two) (format "Full house, %s and %s"
			      (poker-rank-value-name-plural three)
			      (poker-rank-value-name-plural two)))
    (`(5 ,high ,_ ,_ ,_ ,_) (format "%s high flush"
				    (poker-rank-value-name high)))
    (`(4 ,high) (pcase high
		  (3 "5 high straight (steel wheel)")
		  (_ (format "%s high straight" (poker-rank-value-name high)))))
    (`(3 ,three ,high ,_) (format "Three %s, %s high"
				  (poker-rank-value-name-plural three)
				  (poker-rank-value-name high)))
    (`(2 ,two1 ,two2 ,high) (format "Two pairs of %s and %s, %s high"
				    (poker-rank-value-name-plural two1)
				    (poker-rank-value-name-plural two2)
				    (poker-rank-value-name high)))
    (`(1 ,two ,high ,_ ,_) (format "One pair of %s, %s high"
				    (poker-rank-value-name-plural two)
				   (poker-rank-value-name high)))
    (`(0 ,high ,k1 ,k2 ,k3 ,k4) (format "High card %s, %s %s %s %s kickers"
					(poker-rank-value-name high)
					(poker-rank-value-name k1)
					(poker-rank-value-name k2)
					(poker-rank-value-name k3)
					(poker-rank-value-name k4)))))

(defun poker-full-deck ()
  (cl-loop for rank in poker-ranks
	   nconc (cl-loop for suit in poker-suits
			  collect (poker-make-card rank suit))))

(defun poker-random-deck ()
  (append (cookie-shuffle-vector (apply 'vector (poker-full-deck))) nil))

(defun poker-simulate-holdem-showdown (player-count)
  (cl-assert (<= player-count 22))
  (let ((deck (poker-random-deck))
	(players (make-vector player-count nil))
	(board ()))
    (dotimes (cards 2)
      (dotimes (player player-count)
	(push (pop deck) (aref players player))))
    (pop deck)
    (dotimes (cards 3) (push (pop deck) board))
    (pop deck)
    (push (pop deck) board)
    (pop deck)
    (push (pop deck) board)

    (poker-sort-hands
     (mapcar (lambda (pocket) (poker-best-hand (append pocket board)))
	     players))))

(defun poker-test-hand-frequencies (rounds players)
  (let ((scores ()))
    (dotimes (_ rounds)
      (dolist (hand (poker-simulate-holdem-showdown players))
	(let ((value (poker-hand-value hand)))
	  (let ((elem (assoc value scores)))
	    (if elem
		(cl-incf (cdr elem))
	      (setq scores (append (list (cons value 1)) scores)))))))
    (sort scores
	  (lambda (lhs rhs) (not (version-list-< (car lhs) (car rhs)))))))

(provide 'poker)
;;; poker.el ends here
