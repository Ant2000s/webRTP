FROM erlang:21.3

# Set working directory
WORKDIR /app

# Copy Erlang application
COPY . .

# Installation software...
RUN apt-get update

# ...and libs
RUN apt-get install -y ffmpeg
RUN apt-get install -y libortp-dev
RUN rebar3 upgrade
# Expose relevant port
EXPOSE 8080

#Starting point
ENTRYPOINT ["rebar3","shell"]