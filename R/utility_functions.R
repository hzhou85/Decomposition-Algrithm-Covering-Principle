#' A Function for Construction of a Sets of Dominance Relations
#'
#' This function allows you to construct a subrule.
#' @param Ii, an index set of dominated null hypotheses.
#' @param Ji, an index set of dominant null hypotheses. Ii<Ji
#' @keywords Dominance Relation, Covering Principle, Multiple Testing, Multiple Comparisons
#' @export a list, Maximum Unconstrained Class (MUC)
#' @examples
#'
#' Example 5 in CP paper (Li & Zhou 2019)
#' n<-6
#' d1<-subrule(c(3),c(1))
#' d2<-subrule(c(5),c(3))
#' d3<-subrule(c(5),c(1))
#' d4<-subrule(c(6),c(4))
#' d5<-subrule(c(4),c(2))
#' d6<-subrule(c(6),c(2))
#' D_rule<-list(d1,d2,d3,d4,d5,d6)
#' MaxUnconstClass(n,D_rule)


subrule<-function(Ii,Ji)
{
  rule_i<-list(I=Ii,J=Ji)
  rule_i
}


#' A Function to Construct an Operator for Verfication of Subsets
#'
#' This function allows you to verify whether a set "u" is a subset of a set "v".
#' @param u, a set
#' @param v, a set
#' @keywords
#' @export T if "u" is a subset of "v"; otherwise, F.
#' @examples
#' u<-c(1,2)
#' v<-c(1,2,3,4)
#' a<-(u %subset% v)


# %subset% Function:
"%subset%"<-function(u,v){
  return(setequal(intersect(u,v),u))
}
