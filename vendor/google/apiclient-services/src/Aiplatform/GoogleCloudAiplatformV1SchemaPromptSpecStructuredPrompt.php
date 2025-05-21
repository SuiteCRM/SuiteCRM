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

class GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt extends \Google\Collection
{
  protected $collection_key = 'predictionInputs';
  protected $contextType = GoogleCloudAiplatformV1Content::class;
  protected $contextDataType = '';
  protected $examplesType = GoogleCloudAiplatformV1SchemaPromptSpecPartList::class;
  protected $examplesDataType = 'array';
  /**
   * @var string
   */
  public $infillPrefix;
  /**
   * @var string
   */
  public $infillSuffix;
  /**
   * @var string[]
   */
  public $inputPrefixes;
  /**
   * @var string[]
   */
  public $outputPrefixes;
  protected $predictionInputsType = GoogleCloudAiplatformV1SchemaPromptSpecPartList::class;
  protected $predictionInputsDataType = 'array';
  protected $promptMessageType = GoogleCloudAiplatformV1SchemaPromptSpecPromptMessage::class;
  protected $promptMessageDataType = '';

  /**
   * @param GoogleCloudAiplatformV1Content
   */
  public function setContext(GoogleCloudAiplatformV1Content $context)
  {
    $this->context = $context;
  }
  /**
   * @return GoogleCloudAiplatformV1Content
   */
  public function getContext()
  {
    return $this->context;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecPartList[]
   */
  public function setExamples($examples)
  {
    $this->examples = $examples;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecPartList[]
   */
  public function getExamples()
  {
    return $this->examples;
  }
  /**
   * @param string
   */
  public function setInfillPrefix($infillPrefix)
  {
    $this->infillPrefix = $infillPrefix;
  }
  /**
   * @return string
   */
  public function getInfillPrefix()
  {
    return $this->infillPrefix;
  }
  /**
   * @param string
   */
  public function setInfillSuffix($infillSuffix)
  {
    $this->infillSuffix = $infillSuffix;
  }
  /**
   * @return string
   */
  public function getInfillSuffix()
  {
    return $this->infillSuffix;
  }
  /**
   * @param string[]
   */
  public function setInputPrefixes($inputPrefixes)
  {
    $this->inputPrefixes = $inputPrefixes;
  }
  /**
   * @return string[]
   */
  public function getInputPrefixes()
  {
    return $this->inputPrefixes;
  }
  /**
   * @param string[]
   */
  public function setOutputPrefixes($outputPrefixes)
  {
    $this->outputPrefixes = $outputPrefixes;
  }
  /**
   * @return string[]
   */
  public function getOutputPrefixes()
  {
    return $this->outputPrefixes;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecPartList[]
   */
  public function setPredictionInputs($predictionInputs)
  {
    $this->predictionInputs = $predictionInputs;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecPartList[]
   */
  public function getPredictionInputs()
  {
    return $this->predictionInputs;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecPromptMessage
   */
  public function setPromptMessage(GoogleCloudAiplatformV1SchemaPromptSpecPromptMessage $promptMessage)
  {
    $this->promptMessage = $promptMessage;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecPromptMessage
   */
  public function getPromptMessage()
  {
    return $this->promptMessage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt');
