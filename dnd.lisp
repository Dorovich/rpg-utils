;; DATOS DE DRAGONES Y MAZMORRAS

(defparameter *dnd-data* (make-hash-table :test 'equalp))

;; Definiciones

(defclass dnd/class ()
  ((name
    :initarg :name
    :accessor dnd/class.name)
   (hit-dice
    :initarg :hitdie
    :accessor dnd/class.hitdie)
   (proficiencies
    :initarg :profs
    :accessor dnd/class.profs)
   (features
    :initarg :features
    :accessor dnd/class.features)))

(defmacro add-class (name hd (armor weapon tools saves skills) features)
  `(setf (gethash ,name *dnd-data*)
         (make-instance 'dnd/class
                        :name ,name
                        :hd ,hd
                        :profs (vector ,armor ,weapon ,tools ,saves ,skills)
                        :features ,features)))

(defmethod print-thing ((thing dnd/class)))

(defclass dnd/race ()
  ((name
    :initarg :name
    :accessor dnd/race.name)
   (ability-scores
    :initarg :scores
    :accessor dnd/race.scores)
   (size
    :initarg :size
    :accessor dnd/race.size)
   (speed
    :initarg :speed
    :accessor dnd/race.speed)))

(defmacro add-race (name scores size speed)
  `(setf (gethash ,name *dnd-data*)
         (make-instance 'dnd/race
                        :name ,name
                        :scores ,scores
                        :size ,size
                        :speed ,speed)))

(defmethod print-thing ((thing dnd/race)))

(defclass dnd/character ()
  ((name
    :initarg :name
    :accessor dnd/character.name)
   (level
    :initarg :level
    :accessor dnd/character.name)
   (hit-points
    :initarg :hp
    :accessor dnd/character.hp)
   (race
    :initarg :race
    :accessor dnd/character.race)
   (class
    :initarg :class
    :accessor dnd/character.class)))

(defmethod print-thing ((thing dnd/character)))

;; Clases

(add-class "Rogue" "d8"
           '(("Light Armor") ;armor
             ("Simple Weapons" "Hand Crossbow" "Longsword" "Rapier" "Shortsword") ; weapons
             ("Thieve's Tools") ; tools
             ("Dexterity" "Intelligence") ; saves
             ("Acrobatics" "Athletics" "Deception" "Insight" "Intimidation" "Investigation" "Perception" "Performance" "Persuassion" "Sleight of Hand" "Stealth")) ; skills
           '(("Expertise" "Sneak Attack" "Thieve's Cant") ; lvl 1
             ("Cunning Action") ; lvl 2
             ("Roguish Archetype" "Steady Aim") ; lvl 3
             ))
