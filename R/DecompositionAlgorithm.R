#' A Decomposition Algorithm Function
#'
#' This function MaxUnconstClass(n,D) allows you to decompose the whole family of n null hypotheses
#' into a few overlapped subsets or Maximum Unconstrained Class (MUC).
#' @param n, number of all null hypotheses.
#' @param D, a list of m dominance relations
#' @param output, a list of overlapped subsets
#' @keywords Dominance Relation, Covering Principle, Multiple Testing, Multiple Comparisons
#' @export a list, Maximum Unconstrained Class (MUC)
#' @examples
#' # Example 5 in paper (Li & Zhou 2019) Communications in Statistics - Theory and Methods
#' # "A new approach to address multiplicity in hypothesis testing with constraints"
#' #
#'
#' n<-6
#' d1<-subrule(c(3),c(1))
#' d2<-subrule(c(5),c(3))
#' d3<-subrule(c(5),c(1))
#' d4<-subrule(c(6),c(4))
#' d5<-subrule(c(4),c(2))
#' d6<-subrule(c(6),c(2))
#' D_rule<-list(d1,d2,d3,d4,d5,d6)
#' MaxUnconstClass(n,D_rule)
#' # It returs a list of 8 subsets as follows:
#' # {(1,2),(1,4),(1,6),(2,3),(2,5),(3,4),(3,6),(4,5),(5,6)}


# Combinations:-----
#
# Level 1:select n-1 out n; each column is a combination
# Number of columns = number of all combinations: choose(3,2)=3

# Function to find Maximum Unconstrained Class -----
MaxUnconstClass<-function(n,D)
{
# RemainderSet: remainder class storing in-decompsiable subsets
  D_rule<-D
  n_rules<-length(D_rule)
  LL<-list()

# Generate RemainderSet for 1-level; Only two rules are needed: (1)J in S (2) I and S <> empty
# Define RemainderSet as global list-variable----
  RemainderSet<-list()
  L1<-combn(n,n-1)
# Convert matrix of all combinations combn(n,k) into a list LL
  LL<-as.list(data.frame(L1))
  nn<-length(LL)
  IndexSet<-c()


# Level-1: J in S and I intersection S <> Empty set ------

for (i4 in 1:nn)
{
	S<-LL[[i4]]
	for (i5 in 1:n_rules)
	{
		I<-D_rule[[i5]]$I
		J<-D_rule[[i5]]$J
		if ( (J %subset% S)  && (length(intersect(I,S))!=0) )
    {  #  cat("J in S; I and S not Empty",i4,"\n")
			IndexSet<-c(IndexSet,i4)
			break
		}
  }

}
Temp<-list()
if (length(IndexSet)!=0 )
{
  for (ii1 in 1:length(IndexSet))
  { tt<-IndexSet[ii1]
	  TT<-LL[[tt]]
	  Temp<-c(Temp,list(TT))
  }
}
else
{
  RemainderSet<-setdiff(LL,Temp)
}
####################################################################################################
# Function "Remain" to get the Remainder Set: LL for k-level;
# k = n-2,n-3,...,2,1
#Remain<-function(n,k)
##########################################################################
j1<-(n-2)
while (j1>= 1)
{
#cat("Length of RemainderSet",length(RemainderSet),"\n")
	k<-j1
	L1<-combn(n,k)
	# Convert matrix of all combinations combn(n,k) into a list LL
	LL<-as.list(data.frame(L1))
	nn<-length(LL) # number of combinations c(n,k), k=n-2,n-3,...2,1
	IndexSet<-c()
	for (i4 in 1:nn)
	{
  	S<-LL[[i4]]
##################################################
# First: Check if "S" is a subset of RemainderSet
#
##################################################
		nnn<-length(RemainderSet)
		if (nnn!=0)
		{
			for (i5 in 1:nnn)
			{	if (S %subset% RemainderSet[[i5]])
				{	# If S is a subset of RemainderSet, delete this subset in LL
				IndexSet<-c(IndexSet,i4)
				break
				}
			}
		}
####################################################################
# Second: Check if "S" satisfies (1) J in S
#                                (2) intersection S <> Empty set
# "S" is a constrained set if both conditions are satisfied!
####################################################################
		if (length(n_rules)!=0)
		{
			for (i_d in 1:n_rules)
			{
				I<-D_rule[[i_d]]$I
				J<-D_rule[[i_d]]$J
				if ( (J %subset% S)  && (length(intersect(I,S))!=0) )
      	{    	#cat("J in S; I and S not Empty",i4,"\n")
					IndexSet<-c(IndexSet,i4) #IndexSet= index of constrained sets
					break
				}
			}
		}
	}

	Temp1<-list()
	if (length(IndexSet)!=0)
	{
		for (ii1 in 1:length(IndexSet))
		{
		  tt<-IndexSet[ii1]
			TT<-LL[[tt]]
			Temp1<-c(Temp1,list(TT)) #Temp1=all constrained sets at level k
		}
		Temp2<-setdiff(LL,Temp1)   #Temp2=all unconstrained sets at level k
#Append all unconstrained sets at level-k to RemainderSet
		RemainderSet<-c(RemainderSet,Temp2)
	}
	else
	{
	  Temp2<-setdiff(LL,Temp1)
	  RemainderSet<-c(RemainderSet,Temp2)
	}
j1<-j1-1
}
return(RemainderSet)
}
