FROM racket/racket:8.18-full

WORKDIR /app

# Install system deps + Racket packages first (cached layer)
RUN apt-get update && apt-get install -y graphviz 

# Copy source after — changes here won't invalidate the layer above
COPY . .

CMD ["racket", "main.rkt"]