require(AUC)

pred.roc <- roc(pred, as.factor(test.label$label))

plot(pred.roc, main = sprintf('ROC curve. AUC: %.3f', auc(pred.roc)) )

measureMultiLabelF1(test.label$label, pred)
