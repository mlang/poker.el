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
  (+ (* (cl-position suit poker-suits) 13) (cl-position rank poker-ranks)))

(defsubst poker-card-rank (card)
  (cl-check-type card (integer 0 51))
  (% card 13))

(defsubst poker-card-suit (card)
  (cl-check-type card (integer 0 51))
  (/ card 13))

(defsubst poker-card-name (card)
  (cl-check-type card (integer 0 51))
  (concat (aref ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"]
		(poker-card-rank card))
	  (aref ["c" "d" "h" "s"] (poker-card-suit card))))
(defun poker-hand-value (hand)
  "Calculate the value of a given 5 card poker HAND.
The result is a 24 bit integer where the leftmost 4 bits (0-8) indicate the type
of hand, and the remaining nibbles are rank values of decisive cards.
The highest possible value is therefore #x8CBA98 and the lowest is #x053210."
  (let* ((ranks (mapcar #'poker-card-rank hand))
	 (rank-counts (sort (mapcar (lambda (rank) (cons (cl-count rank ranks) rank))
				    (cl-remove-duplicates ranks))
			    (lambda (lhs rhs) (or (> (car lhs) (car rhs))
						  (and (= (car lhs) (car rhs))
						       (> (cdr lhs) (cdr rhs))))))))
    (setq ranks (mapcar #'cdr rank-counts))
    (logior (ash (pcase (mapcar #'car rank-counts)
		   (`(1 1 1 1 1)
		    (let ((straight (or (when (and (eq (nth 0 ranks) 12) (eq (nth 1 ranks) 3))
					  (setq ranks '(3 2 1 0 0)))
					(eq (- (nth 0 ranks) (nth 4 ranks)) 4)))
			  (flush (eq (length (cl-delete-duplicates
					      (mapcar #'poker-card-suit hand))) 1)))
		      (cond ((and straight flush) 8) (flush 5) (straight 4) (t 0))))
		   (`(2 1 1 1) 1) (`(2 2 1) 2) (`(3 1 1) 3) (`(3 2) 6) (`(4 1) 7))
		 20)
	    (ash (nth 0 ranks) 16)
	    (ash (nth 1 ranks) 12)
	    (if (> (length ranks) 2) (ash (nth 2 ranks) 8) 0)
	    (if (> (length ranks) 3) (ash (nth 3 ranks) 4) 0)
	    (if (> (length ranks) 4) (nth 4 ranks) 0))))

(defun poker-hand-> (hand1 hand2)
  (> (poker-hand-value hand1) (poker-hand-value hand2)))

(defun poker-sort-hands (hands)
  (mapcar #'cdr
	  (cl-sort (mapcar (lambda (hand) (cons (poker-hand-value hand) hand)) hands)
		   #'> :key #'car)))

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

(defconst poker-full-deck (cl-loop for card from 0 to 51 collect card))

(defun poker-random-deck ()
  (append (cookie-shuffle-vector (apply 'vector poker-full-deck)) nil))

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

(defun poker-pocket-strength (pocket &optional community opponents)
  (let ((wins 0) (iterations 200))
    (dotimes (i iterations)
      (let ((deck (poker-random-deck))
	    (players (make-vector (or opponents 1) nil)))
	(dolist (card pocket) (setq deck (delete card deck)))
	(dolist (card community) (setq deck (delete card deck)))
	(dotimes (cards 2)
	  (dotimes (player (or opponents 1))
	    (push (pop deck) (aref players player))))
	(let ((board (append community nil)))
	  (dotimes (_ (- 5 (length community)))
	    (push (pop deck) board))
	  (setq wins (+ wins (caar (cl-sort
				    (mapcar (lambda (info)
					      (setcdr info (poker-best-hand (append (cdr info) board)))
					      info)
					    (append (list (cons 1 pocket))
						    (mapcar (lambda (cards)
							      (cons 0 cards))
							    players)))
				    #'poker-hand-> :key #'cdr)))))))
    (/ (float wins) iterations)))

(defun poker-pre-flop-starting-hands (opponents)
  (let ((rank-name (vector "2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"))
	(hands ()))
    (dolist (rank1 poker-ranks)
      (dolist (rank2 poker-ranks)
	(if (eq rank1 rank2)
	  (push (cons (poker-pocket-strength (list (poker-make-card rank1 'clubs)
						   (poker-make-card rank2 'hearts))
					     nil opponents)
		      (if (memq rank1 '(2 3 4 5 6 7 8 9))
			  (+ (* rank1 10) rank1)
			(intern (format "%s%s" (aref rank-name (poker-rank-value rank1))
				      (aref rank-name (poker-rank-value rank2))))))
		hands)
	  (when (< (poker-rank-value rank1) (poker-rank-value rank2))
	    (let ((tmp rank1))
	      (setq tmp rank1
		    rank1 rank2
		    rank2 tmp)))
	  (dolist (suited '(nil t))
	    (let ((code (if (and (memq rank1 '(2 3 4 5 6 7 8 9))
				 (memq rank2 '(2 3 4 5 6 7 8 9))
				 (not suited))
			    (+ (* rank1 10) rank2)
			  (intern (format "%s%s%s"
					  (aref rank-name (poker-rank-value rank1))
					  (aref rank-name (poker-rank-value rank2))
					  (if suited "s" ""))))))
	      (unless (rassq code hands)
		(accept-process-output)
		(message "%S" code)
		(push (cons (if suited
				(poker-pocket-strength
				 (list (poker-make-card rank1 'clubs)
				       (poker-make-card rank2 'clubs))
				 nil opponents)
			      (poker-pocket-strength
			       (list (poker-make-card rank1 'clubs)
				     (poker-make-card rank2 'hearts))
			       nil opponents))
			    code)
		      hands)))))))
    (cl-sort hands #'> :key #'car)))

(defun poker-pot-odds (bet pot)
  (/ (float bet) (+ bet pot)))

(defun poker-fold-call-raise (fold call raise)
  (cl-assert (= (+ fold call raise) 100))
  (let ((value (random 100)))
    (cond
     ((< value fold) 'fold)
     ((< value (+ fold call)) 'call)
     ((< value (+ fold call raise)) 'raise))))


(defun poker-make-player (name fcr-fn)
  (list (cons 'name name)
	(cons 'stack 0)
	(cons 'wagered 0)
	(cons 'pocket nil)
	(cons 'fcr-fn fcr-fn)))
  
(defun poker-player-name (player)
  (cdr (assq 'name player)))

(defun poker-player-stack (player)
  (cdr (assq 'stack player)))

(defun poker-player-bet (player amount)
  (let ((actual (min (poker-player-stack player) amount)))
    (cl-assert (> actual 0))
    (cl-decf (cdr (assq 'stack player)) actual)
    (cl-incf (cdr (assq 'wagered player)) actual)
    actual))

(defun poker-player-payout (player amount)
  (cl-incf (cdr (assq 'stack player)) amount)
  amount)

(defun poker-player-wagered (player)
  (cdr (assq 'wagered player)))

(defun poker-player-pocket (player)
  (cdr (assq 'pocket player)))

(defun poker-player-fold (player)
  (setcdr (assq 'pocket player) nil))

(defun poker-player-active-p (player)
  (and (poker-player-pocket player) (> (poker-player-wagered player) 0)))

(defun poker-player-all-in-p (player)
  (and (poker-player-active-p player) (zerop (poker-player-stack player))))

(defun poker-player-best-hand (player community)
  (poker-best-hand (append (poker-player-pocket player) community)))

(defun poker-player-give-card (player card)
  (cl-check-type card (integer 0 51))
  (push card (cdr (assq 'pocket player))))

(defun poker-player-fcr-fn (player)
  (cdr (assq 'fcr-fn player)))

(defun poker-player-fcr (player pot due board &optional opponents)
  (funcall (poker-player-fcr-fn player) player pot due board opponents))

(defun poker-interactive-fcr (player pot due board &optional opponents)
  (let ((char nil))
    (while (not (memq char '(?c ?f ?r)))
      (setq char (read-char
		  (format "%s, %d stack, %d in pot, %d to call: (c)all, (f)old or (r)aise? "
			  (mapconcat #'poker-card-name
				     (append (poker-player-pocket player) board)
				     " ")
			  (poker-player-stack player) pot due))))
    (cdr (assq char '((?c . call) (?f . fold) (?r . raise))))))

(defun poker-automatic-fcr (player pot due board &optional opponents)
  (let* ((strength (poker-pocket-strength (poker-player-pocket player) board opponents))
	 (pot-odds (poker-pot-odds due pot))
	 (rate-of-return (/ strength pot-odds))
	 (action (cond
		  ((< rate-of-return 0.8) (poker-fold-call-raise 95 1 4))
		  ((< rate-of-return 1.0) (poker-fold-call-raise 80 15 5))
		  ((< rate-of-return 1.3) (poker-fold-call-raise 0 60 40))
		  (t (poker-fold-call-raise 0 25 75)))))
    (when (and (memq action '(call raise))
	       (< (- (poker-player-stack player) due) 200) (< strength 0.5))
      (setq action 'fold))
    (when (and (eq action 'raise) (< strength 0.1))
      (setq action 'call))
    (when (and (zerop due) (eq action 'fold))
      (setq action 'call))
    (message "%f/%f==%f, %S" strength pot-odds rate-of-return action)
    action))

(defun poker-count-active-players (players)
  (cl-count 0 players :test #'< :key #'poker-player-stack))

(defun poker-rotate-to-first (player players)
  (let ((position (cl-position player players)))
    (when position
      (let ((shift (- (length players) position)))
	(append (last players shift) (butlast players shift))))))

(defun poker-next-players (player players)
  (cdr (poker-rotate-to-first player players)))

(defun poker-next-player (player players)
  (car (poker-next-players player players)))

(defun poker-pot (players)
  (apply #'+ (mapcar #'poker-player-wagered players)))

(defun poker-current-wager (players)
  (apply #'max (mapcar #'poker-player-wagered players)))

(defun poker-collect-wager (amount players)
  (let ((total 0))
    (dolist (player players total)
      (let ((wagered (assq 'wagered player)))
	(if (> amount (cdr wagered))
	    (progn
	      (setq total (+ total (cdr wagered)))
	      (setcdr wagered 0))
	  (setq total (+ total amount))
	  (setcdr wagered (- (cdr wagered) amount)))))))

(defun poker-distribute-winnings (winners players)
  (cl-assert (not (null winners)))
  (cl-assert (> (length players) 1))
  (if (= (length winners) 1)
      (poker-player-payout (car winners)
			   (poker-collect-wager (poker-player-wagered (car winners))
						players))
    (let* ((lowest (apply #'min (mapcar #'poker-player-wagered winners)))
	   (total (poker-collect-wager lowest players))
	   (each (/ total (length winners)))
	   (leftover (- total (* each (length winners)))))
      (poker-player-payout (car winners) (+ each leftover))
      (dolist (player (cdr winners)) (poker-player-payout player each))
      total)))

(defun poker-dealer (min-bet deck board button-player players)
  "Deal a round of texas holdem poker for PLAYERS."
  (cl-assert (> (length players) 1))
  (cond
   ;; pre-flop
   ((and (null board) (zerop (poker-pot players)))
    (let ((blinds (poker-rotate-to-first button-player players)))
      (message "Collecting blinds.")
      (poker-player-bet (car blinds) (/ min-bet 2))
      (poker-player-bet (cadr blinds) min-bet)
      (message "Dealing cards to players.")
      (dotimes (_ 2)
	(dolist (player players) (poker-player-give-card player (pop deck))))

      (message "Initial betting round.")
      (dolist (player (poker-next-players (cadr blinds) players))
	(let* ((amount-to-call (- (poker-current-wager players)
				  (poker-player-wagered player)))
	       (opponents (1- (length (cl-remove-if-not #'poker-player-pocket
							players))))
	       (decision (poker-player-fcr player (poker-pot players)
					   amount-to-call board opponents)))
	  (message "%s %S" (poker-player-name player) decision)
	  (cond
	   ((eq decision 'call) (when (> amount-to-call 0)
				  (poker-player-bet player amount-to-call)))
	   ((eq decision 'raise)
	    (message "%s raises by %d." (poker-player-name player) min-bet)
	    (poker-player-bet player (+ amount-to-call min-bet)))
	   ((eq decision 'fold) (poker-player-fold player)))))

      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered (cadr blinds))))
	     (decision (poker-player-fcr (cadr blinds) (poker-pot players)
					 amount-to-call board)))
	(message "%s %S" (poker-player-name (cadr blinds)) decision)
	(cond
	 ((eq decision 'call)
	  (when (> amount-to-call 0)
	    (poker-player-bet (cadr blinds) amount-to-call)))
	 ((eq decision 'raise)
	  (poker-player-bet (cadr blinds) (+ amount-to-call min-bet)))
	 ((eq decision 'fold)
	  (poker-player-fold (cadr blinds)))))

      (poker-dealer min-bet deck board button-player players)))

   ;; All but one have folded
   ((and (not (zerop (poker-pot players)))
	 (= (length (cl-remove-if-not #'poker-player-active-p players)) 1))
    (let ((winners (cl-remove-if-not #'poker-player-active-p players)))
      (message "%s silently wins %d."
	       (poker-player-name (car winners))
	       (poker-distribute-winnings winners players))
      winners))
   
   ;; pre-flop, second round of bets, no raises allowed
   ((and (null board) (cl-remove-if (lambda (player) (or (zerop (poker-player-wagered player))
							 (not (poker-player-pocket player))
							 (= (poker-player-wagered player)
							    (poker-current-wager players))))
				    (poker-next-players
				     (poker-next-player button-player players)
				     players)))

    (message "Pre flop, second round of bets.")

    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (zerop (poker-player-wagered player))
			   (not (poker-player-pocket player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     (poker-next-players (poker-next-player button-player players)
					 players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	  (message "%s wants to %S" (poker-player-name player) decision)
	  (cond
	   ((eq decision 'call) (when (> amount-to-call 0)
				  (poker-player-bet player amount-to-call)))
	   ((eq decision 'raise)
	    (message "no raise allowed, calling." (poker-player-name player))
	    (when (> amount-to-call 0)
	      (poker-player-bet player amount-to-call)))
	   ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; flop
   ((null board)
    (sit-for 1)
    (dotimes (_ 3) (push (pop deck) board))

    (message "The flop: %s" (mapconcat #'poker-card-name board " "))
    (sit-for 2)

    (dolist (player (cl-remove-if-not #'poker-player-active-p
				      (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	(message "%s %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "%s raises by %d." (poker-player-name player) min-bet)
	  (poker-player-bet player (+ amount-to-call min-bet)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; flop, second round of bets, no raises allowed
   ((and (= (length board) 3) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-active-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       (poker-rotate-to-first button-player players)))
    (message "The flop, second round of bets.")
    (dolist (player (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-active-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	(message "%s wants to %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "no raise allowed, %s is calling." (poker-player-name player))
	  (when (> amount-to-call 0)
	    (poker-player-bet player amount-to-call)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; turn
   ((= (length board) 3)
    (sit-for 1)
    (push (pop deck) board)

    (message "The turn: %s" (mapconcat #'poker-card-name board " "))
    (sit-for 2)

    (setq min-bet (* min-bet 2))

    (dolist (player (cl-remove-if-not #'poker-player-active-p
				      (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	(message "%s %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "%s raises by %d." (poker-player-name player) min-bet)
	  (poker-player-bet player (+ amount-to-call min-bet)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; turn, second round of bets, no raises allowed
   ((and (= (length board) 4) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-active-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       (poker-rotate-to-first button-player players)))
    (message "The turn, second round of bets.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-active-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	(message "%s wants to %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "no raise allowed, %s is calling." (poker-player-name player))
	  (when (> amount-to-call 0)
	    (poker-player-bet player amount-to-call)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; river
   ((= (length board) 4)
    (push (pop deck) board)
    (message "The river: %s" (mapconcat #'poker-card-name board " "))

    (dolist (player (cl-remove-if-not #'poker-player-active-p
				      (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (opponents (1- (length (cl-remove-if-not #'poker-player-active-p players))))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board opponents)))
	(message "%s %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "%s raises by %d." (poker-player-name player) min-bet)
	  (poker-player-bet player (+ amount-to-call min-bet)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; river, second round of bets, no raises allowed
   ((and (= (length board) 5) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-active-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       (poker-rotate-to-first button-player players)))
    (message "Last betting round.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-active-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     (poker-rotate-to-first button-player players)))
      (let* ((amount-to-call (- (poker-current-wager players)
				(poker-player-wagered player)))
	     (decision (poker-player-fcr player (poker-pot players)
					 amount-to-call board)))
	(message "%s wants to %S" (poker-player-name player) decision)
	(cond
	 ((eq decision 'call) (when (> amount-to-call 0)
				(poker-player-bet player amount-to-call)))
	 ((eq decision 'raise)
	  (message "no raise allowed, %s is calling." (poker-player-name player))
	  (when (> amount-to-call 0)
	    (poker-player-bet player amount-to-call)))
	 ((eq decision 'fold) (poker-player-fold player)))))

    (poker-dealer min-bet deck board button-player players))

   ;; showdown
   ((= (length board) 5)
    (cl-assert (not (zerop (poker-pot players))))
    (let ((in-play (cl-remove-if-not #'poker-player-active-p players))
	  (groups ()))
      (cl-assert (> (length in-play) 1))
      (while (> (length in-play) 1)
	(if (= (length in-play) 1)
	    (progn
	      (message "%s wins %d."
		       (poker-player-name (car in-play))
		       (poker-distribute-winnings in-play players))
	      (push in-play groups)
	      (setq in-play nil))
	  (let* ((best-hand-value (poker-hand-value
				   (car
				    (poker-sort-hands
				     (mapcar (lambda (player)
					       (poker-player-best-hand player board))
					     in-play)))))
		 (winners (cl-remove-if (lambda (player)
					  (< (poker-hand-value
					      (poker-player-best-hand player board))
					     best-hand-value))
					in-play)))
	    (poker-distribute-winnings winners players)
	    (push winners groups))
	  (setq in-play (cl-remove-if-not #'poker-player-active-p players))))

      (cons board (nreverse groups))))

   (t (list 'error small-blind big-blind deck board button-player players))))

(defun poker (initial-stack min-bet players)
  "Play a game of texas holdem poker."
  (interactive (list (read-number "Initial stack: " 1000)
		     (read-number "Minimum bet: " 50)
		     (list (poker-make-player "Angela" #'poker-automatic-fcr)
			   (poker-make-player "Betina" #'poker-automatic-fcr)
			   (poker-make-player "Christina" #'poker-automatic-fcr)
			   (poker-make-player "Daniela" #'poker-automatic-fcr)
			   (poker-make-player "Emil" #'poker-automatic-fcr)
			   (poker-make-player "Frank" #'poker-automatic-fcr)
			   (poker-make-player "Günter" #'poker-automatic-fcr)
			   (poker-make-player "Hannes" #'poker-automatic-fcr)
			   (poker-make-player "Ingrid" #'poker-automatic-fcr)
			   (poker-make-player (user-full-name) #'poker-interactive-fcr))))
  (cl-assert (> (length players) 1))
  (dolist (player players)
    (message "%s receives %d chips." (poker-player-name player) 1000)
    (setcdr (assq 'stack player) 1000))
  (let ((button-player (nth (random (length players)) players))
	(rounds ())
	(losers ()))
    (while (and button-player (< (length rounds) 4))
      (message "Round %d." (1+ (length rounds)))
      (push (poker-dealer min-bet (poker-random-deck) () button-player players)
	    rounds)
      (mapc #'poker-player-fold players)
      (setq button-player
	    (cadr (cl-remove-if (lambda (player) (zerop (poker-player-stack player)))
			       (poker-rotate-to-first button-player players))))
      (let ((lost (cl-remove-if-not (lambda (player) (zerop (poker-player-stack player)))
				    players)))
	(when lost
	  (setq players (cl-remove-if (lambda (player) (member player lost))
				      players))))
      (message "Remaining players: %s" (mapconcat #'poker-player-name (cl-sort players #'> :key #'poker-player-stack) " "))
      (when button-player
	(setq players (poker-rotate-to-first button-player players))))

    (cons players rounds)))

(provide 'poker)
;;; poker.el ends here
