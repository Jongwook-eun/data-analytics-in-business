## CNN (Convolutional-pooling cycle(convolution-relu activation-max pooling)통해 이미지label예측)

#install.packages("torch")
#install.packages("luz")
#install.packages("torchvision")
library(torch)
library(luz)
library(torchvision)

#Function to transform images to tensor data structures
transform <- function(x) {
  transform_to_tensor(x)
}

# Download and transform the images from the CIFAR-100 database (3*32*32 RGB picture, 카테고리 100개, 50000장 practice, 10000장 test)
train_ds <- cifar100_dataset(root="./", train=TRUE, download=TRUE, transform=transform)
test_ds <- cifar100_dataset(root="./", train=FALSE, transform=transform)

str(train_ds[18]) #y값은 label
length(train_ds)
length(test_ds)

# "fine_label_names.txt" contains the names of all 100 fine categories
labels <- read.table("fine_label_names.txt")
labels <- labels$V1
labels[1:100]

# first 25 images displayed in row major order / their lables
par(mar=c(0,0,0,0), mfrow = c(5,5))
for (i in 1:25)
  plot(as.raster(as.array(train_ds[i][[1]]$permute(c(2,3,1)))))

cat.indx <- sapply(1:25, function(x) train_ds[x][[2]])
matrix(labels[cat.indx],5,5,byrow=TRUE)

# Set up one Convolution-Pooling cycle
conv_block <- nn_module(
  initialize = function(in_channels, out_channels) {
    self$conv <- nn_conv2d(
      in_channels = in_channels,
      out_channels = out_channels,
      kernel_size = c(3,3), #3x3 convolution filter
      padding = "same" #padding the convolved image to the same size as the import image.
    )
    self$relu <- nn_relu() #Apply ReLU activation function
    self$pool <- nn_max_pool2d(kernel_size=c(2,2)) #2x2 block pooling
  },
  forward = function(x) {
    x %>% #차례로 (1번) convolution (2번) apply relu activation (3번) do max pooling
      self$conv() %>%
      self$relu() %>%
      self$pool()
  }
)

# Set up the CNN model
model <- nn_module (
  initialize = function() {
    self$conv <- nn_sequential( # (1~3, convlution+relu+maxpooling을 4번 반복하는 Convolutional-pooling cycles
      conv_block(3,32),
      conv_block(32,64),
      conv_block(64,128),
      conv_block(128,256),
    )
    self$output <- nn_sequential(
      nn_dropout(0.5),
      nn_linear(2*2*256, 512), #one more hidden layer before the output
      nn_relu(),
      nn_linear(512,100) # 출력노드 100개(클래스별로 하나의 노드 배정)
    )
  },
  forward = function(x) {
    x %>%
      self$conv() %>%
      torch_flatten(start_dim = 2) %>% #Flattening
      self$output()
  }
)

# Fit the CNN model (오래걸림)
Sys.time()
fitted <- model %>%
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_rmsprop,
    metric = list(luz_metric_accuracy())
  ) %>%
  set_opt_hparams(lr=0.001) %>%
  fit(
    train_ds,
    epochs = 30, 
    valid_data =0.2,
    dataloader_options = list(batch_size=128) #한 epoch당 312.5개batch((50000*0.8)/128개)
  )
Sys.time()

# To load the pre-fitted CNN model I provided
fitted <- luz_load("cnn_cifar.Luz")

plot(fitted)

#Out-of-sample test
pred <- predict(fitted, test_ds)
pred.class <- torch_argmax(pred,dim=2)
pred.class <- as.array(pred.class)

true.class <- sapply(1:10000, function(x) test_ds[x][[2]])

confusion <- table(pred.class, true.class)
sum(diag(confusion)) / sum(confusion)

# Display some incorrectly predicted images
wrong.list <- which(pred.class!=true.class)
wrong.list[1:100]

par(mar=c(0,0,0,0), mfrow=c(5,5))
for (i in wrong.list[1:25])
  plot(as.raster(as.array(test_ds[i][[1]]$permute(c(2,3,1)))))

cat.indx <- true.class[wrong.list[1:25]]
matrix(labels[cat.indx], 5,5, byrow=TRUE)

cat.indx <- pred.class[wrong.list[1:25]]
matrix(labels[cat.indx], 5,5, byrow=TRUE)
