<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SchemaTextPromptDatasetMetadata extends \Google\Collection
{
  protected $collection_key = 'stopSequences';
  /**
   * @var string
   */
  public $candidateCount;
  /**
   * @var string
   */
  public $gcsUri;
  protected $groundingConfigType = GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfig::class;
  protected $groundingConfigDataType = '';
  /**
   * @var bool
   */
  public $hasPromptVariable;
  /**
   * @var bool
   */
  public $logprobs;
  /**
   * @var string
   */
  public $maxOutputTokens;
  /**
   * @var string
   */
  public $note;
  protected $promptApiSchemaType = GoogleCloudAiplatformV1SchemaPromptApiSchema::class;
  protected $promptApiSchemaDataType = '';
  /**
   * @var string
   */
  public $promptType;
  /**
   * @var bool
   */
  public $seedEnabled;
  /**
   * @var string
   */
  public $seedValue;
  /**
   * @var string[]
   */
  public $stopSequences;
  /**
   * @var string
   */
  public $systemInstruction;
  /**
   * @var string
   */
  public $systemInstructionGcsUri;
  /**
   * @var float
   */
  public $temperature;
  /**
   * @var string
   */
  public $text;
  /**
   * @var string
   */
  public $topK;
  /**
   * @var float
   */
  public $topP;

  /**
   * @param string
   */
  public function setCandidateCount($candidateCount)
  {
    $this->candidateCount = $candidateCount;
  }
  /**
   * @return string
   */
  public function getCandidateCount()
  {
    return $this->candidateCount;
  }
  /**
   * @param string
   */
  public function setGcsUri($gcsUri)
  {
    $this->gcsUri = $gcsUri;
  }
  /**
   * @return string
   */
  public function getGcsUri()
  {
    return $this->gcsUri;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfig
   */
  public function setGroundingConfig(GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfig $groundingConfig)
  {
    $this->groundingConfig = $groundingConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPredictParamsGroundingConfig
   */
  public function getGroundingConfig()
  {
    return $this->groundingConfig;
  }
  /**
   * @param bool
   */
  public function setHasPromptVariable($hasPromptVariable)
  {
    $this->hasPromptVariable = $hasPromptVariable;
  }
  /**
   * @return bool
   */
  public function getHasPromptVariable()
  {
    return $this->hasPromptVariable;
  }
  /**
   * @param bool
   */
  public function setLogprobs($logprobs)
  {
    $this->logprobs = $logprobs;
  }
  /**
   * @return bool
   */
  public function getLogprobs()
  {
    return $this->logprobs;
  }
  /**
   * @param string
   */
  public function setMaxOutputTokens($maxOutputTokens)
  {
    $this->maxOutputTokens = $maxOutputTokens;
  }
  /**
   * @return string
   */
  public function getMaxOutputTokens()
  {
    return $this->maxOutputTokens;
  }
  /**
   * @param string
   */
  public function setNote($note)
  {
    $this->note = $note;
  }
  /**
   * @return string
   */
  public function getNote()
  {
    return $this->note;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptApiSchema
   */
  public function setPromptApiSchema(GoogleCloudAiplatformV1SchemaPromptApiSchema $promptApiSchema)
  {
    $this->promptApiSchema = $promptApiSchema;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptApiSchema
   */
  public function getPromptApiSchema()
  {
    return $this->promptApiSchema;
  }
  /**
   * @param string
   */
  public function setPromptType($promptType)
  {
    $this->promptType = $promptType;
  }
  /**
   * @return string
   */
  public function getPromptType()
  {
    return $this->promptType;
  }
  /**
   * @param bool
   */
  public function setSeedEnabled($seedEnabled)
  {
    $this->seedEnabled = $seedEnabled;
  }
  /**
   * @return bool
   */
  public function getSeedEnabled()
  {
    return $this->seedEnabled;
  }
  /**
   * @param string
   */
  public function setSeedValue($seedValue)
  {
    $this->seedValue = $seedValue;
  }
  /**
   * @return string
   */
  public function getSeedValue()
  {
    return $this->seedValue;
  }
  /**
   * @param string[]
   */
  public function setStopSequences($stopSequences)
  {
    $this->stopSequences = $stopSequences;
  }
  /**
   * @return string[]
   */
  public function getStopSequences()
  {
    return $this->stopSequences;
  }
  /**
   * @param string
   */
  public function setSystemInstruction($systemInstruction)
  {
    $this->systemInstruction = $systemInstruction;
  }
  /**
   * @return string
   */
  public function getSystemInstruction()
  {
    return $this->systemInstruction;
  }
  /**
   * @param string
   */
  public function setSystemInstructionGcsUri($systemInstructionGcsUri)
  {
    $this->systemInstructionGcsUri = $systemInstructionGcsUri;
  }
  /**
   * @return string
   */
  public function getSystemInstructionGcsUri()
  {
    return $this->systemInstructionGcsUri;
  }
  /**
   * @param float
   */
  public function setTemperature($temperature)
  {
    $this->temperature = $temperature;
  }
  /**
   * @return float
   */
  public function getTemperature()
  {
    return $this->temperature;
  }
  /**
   * @param string
   */
  public function setText($text)
  {
    $this->text = $text;
  }
  /**
   * @return string
   */
  public function getText()
  {
    return $this->text;
  }
  /**
   * @param string
   */
  public function setTopK($topK)
  {
    $this->topK = $topK;
  }
  /**
   * @return string
   */
  public function getTopK()
  {
    return $this->topK;
  }
  /**
   * @param float
   */
  public function setTopP($topP)
  {
    $this->topP = $topP;
  }
  /**
   * @return float
   */
  public function getTopP()
  {
    return $this->topP;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaTextPromptDatasetMetadata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaTextPromptDatasetMetadata');
