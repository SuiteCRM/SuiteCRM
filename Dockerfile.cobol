FROM ubuntu:22.04

# Install GnuCOBOL and dependencies
RUN apt-get update && apt-get install -y \
    gnucobol \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Node.js 18
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy package files first for better caching
COPY ./cobol-services/package*.json ./

# Install Node dependencies
RUN npm install

# Copy COBOL services
COPY ./cobol-services .

# Compile COBOL programs
RUN for cob in *.cob; do \
        if [ -f "$cob" ]; then \
            cobc -x -o "${cob%.cob}" "$cob"; \
        fi \
    done

EXPOSE 3000 8080

# Make start script executable
RUN chmod +x start-servers.sh 2>/dev/null || true

# Start both servers
CMD ["bash", "./start-servers.sh"]