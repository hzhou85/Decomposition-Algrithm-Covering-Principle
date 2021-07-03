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

library(dplyr)

subrule<-function(Ii,Ji)
{
  rule_i<-list(I=Ii,J=Ji)
  rule_i
}


# %subset% Function:
"%subset%"<-function(u,v){
  return(setequal(intersect(u,v),u))
}

# Combinations:-----
#
# Level 1:select n-1 out n; each column is a combination
# Number of columns = number of all combinations: choose(3,2)=3

# Function to find Maximum Unconstrained Class -----
MaxUnconstClass<-function(n,D)
{
# RemainderSet: remainder class storing in - decompsiable subsets
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
	for (i5 in seq(1,n_rules))
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
for (ii1 in 1:length(IndexSet))
{     tt<-IndexSet[ii1]
	TT<-LL[[tt]]
	Temp<-c(Temp,list(TT))
}

RemainderSet<-setdiff(LL,Temp)
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
j1<-j1-1
}

return(RemainderSet)

}


# main function ----


#dmnRel_1 <- read.csv("C:/Users/hzhou/R packages/First Shiny app/app4/Demitrienko-2007-Stat_in_Med-1.txt",
#                     header = FALSE,
#                     col.names = paste0("V",seq(1:20)),
#                     fill = TRUE)

#dmnRel_1 <- dmnRel_1[,which(!is.na(dmnRel_1[1,]))]

#which(!is.na(dmnRel_1[1,]))

dmnRel_1 <- read.csv("C:/Users/hzhou/R packages/First Shiny app/app4/Demitrienko-2007-Stat_in_Med.txt",
                     header = FALSE)

dmnRel_1[is.na(dmnRel_1)] <- 0

di <- list()
ii <- 1
for (i in seq(1,(nrow(dmnRel_1)-1),2)) {
  #for (i in seq(1,1)) {
  Ii <- c()
  # print(dmnRel_1[i,])
  #  print(length(dmnRel_1[i,]))
  for (j in 1:length(dmnRel_1[i,])) {
    if (dmnRel_1[i,j] != 0 ) {    Ii <- c(Ii,dmnRel_1[i,j]) }
    # print(i, Ii)
  }
  Ji <- c()
  for (j in 1:length(dmnRel_1[i+1,])) {
    if (dmnRel_1[i+1,j] != 0 ) {    Ji <- c(Ji,dmnRel_1[i+1,j]) }
    # print(Ji)
  }

  di[[ii]]<-list(I = Ii, J = Ji)
  ii <- ii + 1
  #print(di)
}


di

muc <- MaxUnconstClass(8,di)
muc
print(muc)

mx <- max(lengths(muc))
mx
muc_df <- do.call(rbind.data.frame, lapply(muc, `length<-`, mx))
names(muc_df) <- paste('column', 1:mx)
muc_df

