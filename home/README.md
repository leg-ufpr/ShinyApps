Para aparecer o nome na lista da página incial:

1. Copie o aplicativo `home` para o seu diretório `~/ShinyApps`. O
   aplicativo está disponível em: https://github.com/leg-ufpr/ShinyApps
2. Na raíz do diretório `~/ShinyApps` crie um arquivo `index.html` para
   fazer o redirecionamento, com o seguinte conteúdo
   ```
   <META HTTP-EQUIV="Refresh" Content="0; URL=http://shiny.leg.ufpr.br/<username>/home/">
   ```
   onde `<username>` deve ser o seu nome de usuário.
3. (OPCIONAL, mas RECOMENDADO) Na raíz do diretório de cada aplicativo
   crie um arquivo chamado `DESCRIPTION` com **somente** o seguinte
   conteúdo
   ```
   Title: Breve descrição sobre o que faz o aplicativo
   Author: Seu Nome (ou demais autores)
   ```
   Se esse arquivo existir com esse conteúdo, essa descrição irá aparecer
   ao lado do nome do aplicativo na lista de cada usuário.
