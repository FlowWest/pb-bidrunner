FROM 975050180415.dkr.ecr.us-west-2.amazonaws.com/r-for-b4b:latest 

ENV AWS_DEFAULT_REGION=us-west-2

# copy all the R code
COPY . /b4b/
COPY execute.sh b4b/execute.sh
RUN chmod +x b4b/execute.sh

WORKDIR /b4b/ 

CMD ["/b4b/execute.sh"]
