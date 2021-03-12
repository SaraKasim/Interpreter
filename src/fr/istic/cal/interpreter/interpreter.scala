package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */

  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */

  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */

  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */

  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */

  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil    => NlValue
      case h :: t => if (v == h._1) h._2 else lookUp(v, t)
    }
  }

  /**
   * @param v : une variable
   * @param e : une expression
   * @param mem : une mémoire
   * @return la mémoire augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
   * modifiée pour prendre en compte la nouvelle valeur de v sinon
   */

  def assign(v: Variable, e: Value, mem: Memory): Memory = {
    mem match {
      case Nil    => (v, e) :: Nil
      case h :: t => if (v == h._1) (v, e) :: t else h :: assign(v, e, t)
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */

  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl         => NlValue
      case Cst(x)     => CstValue(x)
      case VarExp(x)  => lookUp(Var(x), mem)
      case Cons(x, y) => ConsValue(interpreterExpr(x, mem), interpreterExpr(y, mem))
      case Hd(x) => interpreterExpr(x, mem) match {
        case ConsValue(x, y) => x
        case _               => NlValue
      }
      case Tl(x) => interpreterExpr(x, mem) match {
        case ConsValue(x, y) => y
        case _               => NlValue
      }
      case Eq(x, y) => if (interpreterExpr(x, mem) != interpreterExpr(y, mem)) NlValue else ConsValue(NlValue, NlValue)
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant une expression de cette valeur
   */

  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue         => Nl
      case CstValue(x)     => Cst(x)
      case ConsValue(x, y) => Cons(valueToExpression(x), valueToExpression(y))
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */

  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop         => memory
      case Set(v, exp) => assign(v, interpreterExpr(exp, memory), memory)
      case While(exp, com) => interpreterExpr(exp, memory) match {
        case NlValue => memory
        case _       => interpreterCommand(While(exp, com), interpreterCommands(com, memory))
      }
      case If(exp, com1, com2) => interpreterExpr(exp, memory) match {
        case NlValue => interpreterCommands(com2, memory)
        case _       => interpreterCommands(com1, memory)
      }
      case For(exp, com) =>
        val v = interpreterExpr(exp, memory); v match {
          case NlValue => memory
          case _       => interpreterCommands(com ++ (For(Tl(valueToExpression(v)), com) :: Nil), memory)
        }
    }
  }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */

  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil    => memory
      case h :: t => interpreterCommands(t, interpreterCommand(h, memory))
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */

  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    (vars, vals) match { //It is assumed that the two lists have the same length so we can't have the case of (List(_), Nil) or the case of (Nil, List(_))
      case (Nil, Nil)         => Nil
      case (h :: t, h1 :: t1) => (h, h1) :: interpreterMemorySet(t, t1)
    }
  }

  /**
   * @param vars : une liste décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */

  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    vars match {
      case Nil    => Nil
      case h :: t => lookUp(h, memory) :: interpreterMemoryGet(t, memory)
    }
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */

  def interpreter(program: Program, vals: List[Value]): List[Value] = {

    program match {
      case Progr(in, body, out) => val m = interpreterMemorySet(in, vals); val m2 = interpreterCommands(body, m); interpreterMemoryGet(out, m2)
    }

  }

}