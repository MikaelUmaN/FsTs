FROM mcr.microsoft.com/dotnet/nightly/sdk:5.0

WORKDIR /fsts

# Copy everything and build
COPY . ./
RUN dotnet publish -c Release -o out

# Run tests
RUN dotnet run --project FsTs.Test

ENTRYPOINT ["dotnet", "fsi", "--langversion:preview"]