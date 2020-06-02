library(PKI)

key <- PKI.genRSAkey()
e <- PKI.encrypt(charToRaw("this"),key)
rawToChar(PKI.decrypt(e,key))
rawToChar(PKI.decrypt(e,that))


key <- PKI.genRSAkey()

pubkey  <- PKI.save.key(key,private=F)
privkey <- PKI.save.key(key,private=T)

e <- PKI.encrypt(charToRaw("Something encrypted"),PKI.load.key(pubkey))
save(privkey,file="privkey.rda")
load("privkey.rda",verbose=T)
rawToChar(PKI.decrypt(e,PKI.load.key(privkey)))
