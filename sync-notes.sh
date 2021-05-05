

sudo apt install syncthing

sudo adduser ivan

systemctl enable syncthing@ivan.service
systemctl start syncthing@ivan.service

ssh -L 9999:127.0.0.1:8384 ubuntu@111.222.333.444
