#### johann jacoby, johann.jacoby@gmail.com, last change 201509161027

anonymizeHandles <- function(df, groupings=NULL, ignore=c("-", "---", "--", "", " ", NA), sorting="occurrence", prefixes=NULL, keep=NULL){
			# df: data frame with variables to be anonymized
			# groupings: a list of vectors of variable names, all variable names in one vector are treated together so that values occurring in both variables will get the same anonymized values
			# ignore: values that should benot be anonymized but simply kept in the new data frame
			# sorting: should anonymous values be assigned in order of occurrence ("occurrence") of the original values or alphabetically ("alpha")?
			# prefixes: prefixes for anonymous values; a vector of strings, one string per grouping; in the same order as the groupings (same prefix within one grouping)
			# keep: a vector of values 1 or 0, indicating for each grouping whether the original variables with the original values should be kept (=1) or dropped from the resulting data frame (=0); same order as groupings
			
			
			sorting <- ifelse(sorting %in% c("alpha","occurrence") && !is.null(sorting), sorting, "occurrence")
			print(sorting)
			groupingsnumber<-0; for (i in groupings) {groupingsnumber <- groupingsnumber+length(i)}
			numberOfGroups<-length(groupings)
			if (is.null(keep)) {keep <- rep(1,length(groupings)) }
			for(i in 1:length(groupings)) {
				if (is.null(prefixes[i]) || prefixes[i]=="" || is.na(prefixes[i])) { prefixes[i]<-"ANONID" }
			}
			print(prefixes)
			prefixesfinal <- c()
			for(i in 1:length(prefixes)) {
				if (!prefixes[i] %in% prefixesfinal && table(prefixes)[prefixes[i]] < 2){prefixesfinal <- c(prefixesfinal, prefixes[i])}
				else { 
					for (j in 1:(length(prefixesfinal)+1)) { 
						candidate <- paste0(prefixes[i], sprintf("%02.0f", j))
						cat (candidate,"******\n")
						if (!candidate %in% prefixesfinal) {prefixesfinal[length(prefixesfinal)+1]<-candidate; break; }
					}
				}
				cat(prefixes[i]," ",prefixesfinal[i],"\n")
			}
			prefixesfinal <- paste0(prefixesfinal,"_")
			replacements <- as.data.frame(matrix(NA,ncol=7,nrow=0))
			names(replacements) <- c("grouping.number", "grouping", "original.variable.name", "original.variable.kept", "new.variable.name","original.value","anonymized.value")
			
		#	print(prefixesfinal)
			
			dfnew <- df
			countgroupings <- 1
			cat(".................................................\n")
			cat(">> These values will be ignored: <", paste0(ignore, collapse="> <"),">\n",sep="")
			cat(">> Newly created anonymous values will be prefixed and the original variables kept or removed like this:\n")
			cat("- The original variables *** ", paste(groupings[keep==1|keep==TRUE], collapse=" ")," *** will be KEPT in the resulting data frame and their anonymized version with anonymized values will be added.\n",sep="")
			cat("- The original variables *** ", paste(unlist(groupings[keep==0|keep==FALSE]), collapse=" ")," *** will be REMOVED from the resulting data frame but their anonymized version will be added.\n", sep="")
			cat(">> Anonymous values will be numbered ",ifelse(sorting=="alpha","alphabetcially","according to their order of occurrence"),".\n")
			cat(".................................................\n\n")
			numberofreplacements <- 1
			for (group in groupings){
				vectorVars <- group
				newvarnames <- paste0(group,"_anon")
				helpv <- c()
				for (i in vectorVars){ helpv <- unique(c(helpv, setdiff(as.character(unlist(df[i])), ignore))) }
				if (sorting=="alpha") { helpv <- sort(helpv) }
				newids <- paste0(prefixesfinal[countgroupings]
												 , sprintf(paste0("%0",ceiling(log(length(helpv),10)),".0f"), seq(1:length(helpv)))
												 )
				translatelist <- list()
				for (i in 1:length(helpv)) { translatelist[[length(translatelist)+1]] <- c(helpv[i], newids[i]) }
				
				cat ("\n=====================================\nGrouping ",countgroupings,":\n")
				cat ("\nNEW VARIABLE NAMES (original variables will be ", ifelse(keep[countgroupings] != 1,"DELETED","KEPT"),")\n",sep="")
				cat(paste0("\t\t\t",group, " => ", newvarnames,"\n", collapse=""))
				cat("ANONYMOUS VALUES:\n")
				maptable <- data.frame(matrix(unlist(translatelist), nrow=length(translatelist), byrow=TRUE))
				names(maptable) <- c("original","anonymized")
				print(maptable)
				oldnames <- names(dfnew)
			  for (i in vectorVars){
			  		newvar <- as.character(df[,i])
			  		thisnewvarname <- paste0(i,"_anon")
			  		for (j in translatelist) {
			  			newvar[newvar==j[1]] <- j[2]
			  			replacements[numberofreplacements,] <- c(countgroupings, paste0(group,collapse="|"), i, keep[countgroupings], thisnewvarname, j[1],j[2])
			  			numberofreplacements <- numberofreplacements + 1
			  		}
			  		dfnew <- cbind(dfnew, newvar)
			  }
				names(dfnew) <- c(oldnames, newvarnames)
				countgroupings <- countgroupings + 1
			}
			for (i in 1:length(groupings)) { if (keep[i]!=1) { dfnew <- dfnew[,!names(dfnew) %in% groupings[[i]]]} }	
			#print(unique(replacements[, original.value]))
			replacements2 <- unique(replacements[, c("original.value","grouping","anonymized.value")])
			replacements3 <- unique(replacements[, c("original.variable.name", "original.variable.kept", "new.variable.name")])
			return (list(dataframe=dfnew, replacelog=replacements, uniquereplacements=replacements2, vars=replacements3))
		}
		



######## examples
exReplaceData <- as.data.frame(cbind(v1=as.numeric(c(1,8,1,5,3,1,6,8,8,5,8)),v2=c(8,8,6,4,7,2,6,1,2,4,8),v3=c(6,7,8,1,2,7,3,7,6,6,2),v4=c(6,1,4,6,1,8,8,5,6,4,3),twittername.from=c("D'Glester Hardunkichud","Leoz Maxwell Jilliumz","Ozamataz Buckshank","J'Dinkalage Morgoone","Hingle McCringleberry","Sequester Grundelplith M.D.","Javaris Jamar Javarison-Lamar","Javaris Jamar Javarison-Lamar","Beezer Twelve Washingbeard","The Player Formerly Known as Mousecop","Scoish Velociraptor Maloish"),twittername.to=c("-","J'Dinkalage Morgoone","Beezer Twelve Washingbeard","","J'Dinkalage Morgoone","Beezer Twelve Washingbeard",NA,NA,NA,"Swirvithan L'Goodling-Splatt",NA),v5=c(1,3,1,2,2,5,1,6,4,8,2),v6=c(7,4,2,5,5,5,6,5,4,3,8),v7=c(8,3,5,3,1,7,3,2,8,8,2),someothername=c("Arthur Spooner","Hector Salamanca","Gyp Rosetti","Carmela Soprano","Chandler Muriel Bing","Debra Morgan","Carmela Soprano","Omar Little","Gene Hunt","Omar Little","Thor Gundersen"),someanimal=c("Armadillo","Caribou","Cricket","Armadillo","English Pouter","Gecko","Gecko","Gecko","Quetzal","Sloth","Fiddler Crab"),someanimal2=c("Porpoise","Axolotl","Axolotl","Sloth","Mollusk","Gecko","Wallaby","Firefox","--","Pouter","Caribou"),someanimal3=c(NA,"Gecko","Wallaby","Armadillo","Mollusk","Baboon","","Gecko","Fiddler Crab"," ","Wallaby")))

anonymized.df <- anonymizeHandles(exReplaceData, groupings=list(c("twittername.from", "twittername.to"), "someothername", c("someanimal","someanimal2","someanimal3")), keep=c(1,0,0), sorting="alpha", prefixes=c("firstcluster","secondgroup", "critter"))
anonymized.df[[1]]
anonymized.df$dataframe
anonymized.df[[2]]
anonymized.df$replacelog
anonymized.df[[3]]
anonymized.df$uniquereplacements
anonymized.df[[4]]
anonymized.df$vars

anonymized.df2 <- anonymizeHandles(exReplaceData, groupings=list(c("twittername.from", "twittername.to"), "someothername", c("someanimal","someanimal2","someanimal3")))
anonymized.df2[[1]]
anonymized.df2$dataframe
anonymized.df2[[2]]
anonymized.df2$replacelog
anonymized.df2[[3]]
anonymized.df2$uniquereplacements
anonymized.df2[[4]]
anonymized.df2$vars
