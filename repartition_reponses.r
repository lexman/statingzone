# On commence par se placer dans le répertoire du script
workdir <- getSrcDirectory(function(x) {x})
setwd(workdir)
#install.packages('RSQLite')


library(DBI)
library(tm)
mydb <- dbConnect(RSQLite::SQLite(), "slackingzone.db")

messages <- dbGetQuery(mydb, 'SELECT * from forum')
nbPeres <- rle(sort(messages$Pere))
nbPeres$values[1:30]
nbPeres$lengths[1:30]
# childrenCount = tapply(childrenLength$lengths, childrenLength$values, sum)
# Si Pere == 0, c'est les débuts de threads, on ne veut pas les compter
# Si Pere == -1, il s'agit d'un message supprimé, on ne veut pas le compter ce pere non plus
# Si Pere == 1, C'est une erreur !
# Et puis comme les messages commencent à 17xx..
nbPeresDf <- data.frame(father=nbPeres$values, nb=nbPeres$lengths)
nbChildren <- subset(nbPeresDf, father > 1000)
distribution <- setNames(aggregate(nbChildren$nb, by=list(nbChildren$nb), FUN=length), c("nbChildren", "count"))

# Calcul du nomre de messages sans fils
pere <- messages$Pere
activeMessages <- pere[pere != -1]
nbOrphans = length(activeMessages) - sum(distribution$count)
distribution <- rbind(distribution, c(0, nbOrphans))

plot(distribution$nb, spread$count, log="y", type='h', 
	lwd=20, lend=30, col="light blue",
	main="Combien de reponses ?",
	xlab="Nb de réponses",
	ylab="Nb de messages (éechelle log)"
	)
