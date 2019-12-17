#coding:UTF-8
import sys
import requests

def main():
    url = "https://notify-api.line.me/api/notify"
    token = "PUT_YOUR_TOKEN"
    headers = {"Authorization" : "Bearer "+ token}

    message =  sys.argv[1]
    payload = {"message" :message}

    r = requests.post(url ,headers = headers ,params=payload)

if __name__ == '__main__':
    main()
