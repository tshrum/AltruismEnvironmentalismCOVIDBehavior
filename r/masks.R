

glm1 <- glm(wornMask ~ poly(maskFreqCommunity, 1) + trump, data = d, family = "binomial")
glm2 <-  glm(socialMask ~ poly(maskFreqCommunity, 1), data = d, family = "binomial")
summary(glm1)
coef(glm1)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(glm1))

summary(d$maskFreqCommunity)

(intercept <- coef(glm1)[1])
(b_mask <- coef(glm1)[2])
(logits_mask50 <- intercept + 50 * b_mask)
(logits_mask60 <- intercept + 60 * b_mask)
(logits_mask70 <- intercept + 70 * b_mask)
(logits_mask75 <- intercept + 75  * b_mask)

logit2prob(logits_mask60) - logit2prob(logits_mask50)

logit2prob(logits_mask75) - logit2prob(logits_mask50) 
