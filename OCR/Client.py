from azure.cognitiveservices.vision.computervision import ComputerVisionClient
from azure.cognitiveservices.vision.computervision.models import VisualFeatureTypes
from msrest.authentication import CognitiveServicesCredentials
from PIL import Image
# Get endpoint and key from environment variables
import os

endpoint = os.getenv('https://centralindia.api.cognitive.microsoft.com')
subscription_key = os.getenv('5305f42815ea440bbc50c7817e15bb54')

# Set credentials
ApiKeyCredentials = CognitiveServicesCredentials('5305f42815ea440bbc50c7817e15bb54')

# Create client
client = ComputerVisionClient('https://centralindia.api.cognitive.microsoft.com',ApiKeyCredentials)

