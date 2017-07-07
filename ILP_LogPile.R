# Attempting the log problem
 library(data.table)
 library(Rglpk)
 
 
 n <- 5
 logs <- rbind(c(-1,1,0,-1,0),
               c(1,0,1,0,0),
               c(1,-1,1,0,0),
               c(-1,-1,0,0,-1),
               c(-1,1,0,1,0),
               c(0,1,0,0,1),
               c(1,0,-1,0,-1),
               c(0,-1,0,1,0),
               c(0,0,-1,1,-1),
               c(1,0,-1,0,0))
 
 solve_logs <- function(logs, sols = NULL, print = T) {
   logs <- as.matrix(logs)
   n <- ncol(logs)
   dat <- data.table(expand.grid(Piece = 1:(2*n), Position = 1:(4*n), Location = 1:n^2))
   setkeyv(dat, c("Piece", "Position"))
   
   dat[, Value := c(kronecker(diag(n), logs[Piece,]),
                    kronecker(logs[Piece,], diag(n)),
                    kronecker(diag(n), rev(logs[Piece,])),
                    kronecker(rev(logs[Piece,]), diag(n)) ), by = Piece]
   
   a <- dcast.data.table(dat, Location ~ Piece + Position, value.var = "Value")
   
   b <- c(rep(0, n^2), rep(1, 4*n))
   
   A <- rbind(as.matrix(a)[,-1],
              kronecker(diag(2*n), t(rep(1, 4*n))),
              kronecker(t(rep(1, 4*n)), diag(2*n)) )
   
   dirs <- rep("==", nrow(A))
   types <- rep("B", 8*n)
   
   if(!is.null(sols)) {
     A <- rbind(A, do.call(rbind, lapply(sols, function(x) x$solution)))
     b <- c(b, rep(n, length(sols)))
     dirs <- c(dirs, rep("<", length(sols)))
   }
   
   x <- numeric(ncol(A))
   sol <- Rglpk_solve_LP(x, A, dir = dirs, rhs = b, types = types)
   if(print)
     print(cbind(expand.grid(1:(4*n), 1:(2*n))[,2:1], sol$solution)[sol$solution == 1,])
   invisible(sol)
 }
 
 # Is this necessarily unique? -- No, because of symmetry.
 sol2 <- Rglpk_solve_LP(x, rbind(A, sol$solution), dir = rep("==", nrow(A) + 1), rhs = c(b, 0), types = rep("B", 201))
 expand.grid(1:20, 1:10)[sol2$solution == 1,2:1]
 sol3 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution), dir = rep("==", nrow(A) + 2), rhs = c(b, 0, 0), types = rep("B", 202))
 expand.grid(1:20, 1:10)[sol3$solution == 1,2:1]
 sol4 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution), 
                        dir = rep("==", nrow(A) + 3), rhs = c(b, rep(0, 3)), types = rep("B", 203))
 expand.grid(1:20, 1:10)[sol4$solution == 1,2:1]
 sol5 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution, sol4$solution), 
                        dir = c(rep("==", nrow(A)), rep("<", 4)), rhs = c(b, rep(9, 4)), types = rep("B", 204))
 expand.grid(1:20, 1:10)[sol5$solution == 1,2:1]
 sol6 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution, sol4$solution, sol5$solution), 
                        dir = c(rep("==", nrow(A)), rep("<", 5)), rhs = c(b, rep(9, 5)), types = rep("B", 205))
 expand.grid(1:20, 1:10)[sol6$solution == 1,2:1]
 sol7 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution, sol4$solution, sol5$solution, sol6$solution), 
                        dir = c(rep("==", nrow(A)), rep("<", 6)), rhs = c(b, rep(9, 6)), types = rep("B", 206))
 expand.grid(1:20, 1:10)[sol7$solution == 1,2:1]
 sol8 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution, sol4$solution, sol5$solution, sol6$solution, sol7$solution), 
                        dir = c(rep("==", nrow(A)), rep("<", 7)), rhs = c(b, rep(9, 7)), types = rep("B", 207))
 expand.grid(1:20, 1:10)[sol8$solution == 1,2:1]
 sol9 <- Rglpk_solve_LP(x, rbind(A, sol$solution, sol2$solution, sol3$solution, sol4$solution, sol5$solution, sol6$solution, sol7$solution, sol8$solution), 
                        dir = c(rep("==", nrow(A)), rep("<", 8)), rhs = c(b, rep(9, 8)), types = rep("B", 208))
 expand.grid(1:20, 1:10)[sol9$solution == 1,2:1]
 
 
 
 # Generate puzzle for n = 6
 
 n <- 6
 
 pos <- matrix(n)
 while (max(apply(pos, 1, sum)) > n/2 | max(apply(pos, 2, sum)) > n/2) {
   pos <- matrix(sample(rep(0:1, n^2/2), n^2), n, n)
 }
 
 sgn <- sample(rep(0:1, sum(pos!=0)/2), sum(pos!=0))*2 - 1
 pos[pos != 0] <- sgn
 
 logs <- rbind(pos, -t(pos))
 logs <- data.table(t(apply(logs, 1, function(x) if(abs(x)%*%2^(1:n) > abs(x) %*% 2^(n:1)) rev(x) else x)))
 setkeyv(data.table(logs), paste0("V", 1:n))
 logs[order(abs(V1), abs(V2), abs(V3))]
 
 sol <- list(status = 0)
 sols <- NULL
 i <- 0
 while(sol$status == 0) {
   i <- i + 1
   sol <- solve_logs(logs, sols = sols)
   sols[[i]] <- sol
 }
 if (i > 9)
   print("logs does not have a unique solution")
 
 
 matrix_code <- function(mat) {
   cat(paste0("rbind(", paste0("c(", apply(mat, 1, paste0, collapse = ", "), ")", collapse = ",\n"), " )"))
 }
 
 # A test case
 
 logs_old <- rbind(c(-1, 0, 0, 1, 1, 0),
                   c(1, 0, 0, -1, -1, 0),
                   c(0, -1, -1, 0, 0, 1),
                   c(-1, 0, 0, 1, 0, 1),
                   c(0, -1, -1, 0, 0, 1),
                   c(0, 1, -1, 0, 1, 0),
                   c(1, -1, 0, 1, 0, 0),
                   c(0, 0, 1, 0, 1, -1),
                   c(0, 0, 1, 0, 1, 1),
                   c(-1, 1, 0, -1, 0, 0),
                   c(-1, 1, 0, 0, 0, -1),
                   c(0, 0, -1, -1, -1, 0) )
 solve_logs(logs_old)
 solve_logs(logs_old, sols = lv)
 
 # Chosen configuration
 
 rbind(c(-1, 0, -1, 0, 0, -1),
       c(-1, 1, 0, 0, 1, 0),
       c(1, 1, -1, 0, 0, 0),
       c(0, -1, 1, 1, 0, 0),
       c(1, 0, -1, 1, 0, 0),
       c(1, -1, 0, 0, -1, 0),
       c(1, 1, 0, 0, -1, 0),
       c(1, 0, 1, 0, -1, 0),
       c(1, 0, 0, -1, 1, 0),
       c(0, -1, -1, 1, 0, 0),
       c(1, 0, 0, -1, -1, 0),
       c(1, 0, -1, 0, 0, -1) )
 
 