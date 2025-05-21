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

class GoogleCloudAiplatformV1SchemaPromptApiSchema extends \Google\Collection
{
  protected $collection_key = 'executions';
  /**
   * @var string
   */
  public $apiSchemaVersion;
  protected $executionsType = GoogleCloudAiplatformV1SchemaPromptInstancePromptExecution::class;
  protected $executionsDataType = 'array';
  protected $multimodalPromptType = GoogleCloudAiplatformV1SchemaPromptSpecMultimodalPrompt::class;
  protected $multimodalPromptDataType = '';
  protected $structuredPromptType = GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt::class;
  protected $structuredPromptDataType = '';
  protected $translationPromptType = GoogleCloudAiplatformV1SchemaPromptSpecTranslationPrompt::class;
  protected $translationPromptDataType = '';

  /**
   * @param string
   */
  public function setApiSchemaVersion($apiSchemaVersion)
  {
    $this->apiSchemaVersion = $apiSchemaVersion;
  }
  /**
   * @return string
   */
  public function getApiSchemaVersion()
  {
    return $this->apiSchemaVersion;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptInstancePromptExecution[]
   */
  public function setExecutions($executions)
  {
    $this->executions = $executions;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptInstancePromptExecution[]
   */
  public function getExecutions()
  {
    return $this->executions;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecMultimodalPrompt
   */
  public function setMultimodalPrompt(GoogleCloudAiplatformV1SchemaPromptSpecMultimodalPrompt $multimodalPrompt)
  {
    $this->multimodalPrompt = $multimodalPrompt;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecMultimodalPrompt
   */
  public function getMultimodalPrompt()
  {
    return $this->multimodalPrompt;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt
   */
  public function setStructuredPrompt(GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt $structuredPrompt)
  {
    $this->structuredPrompt = $structuredPrompt;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecStructuredPrompt
   */
  public function getStructuredPrompt()
  {
    return $this->structuredPrompt;
  }
  /**
   * @param GoogleCloudAiplatformV1SchemaPromptSpecTranslationPrompt
   */
  public function setTranslationPrompt(GoogleCloudAiplatformV1SchemaPromptSpecTranslationPrompt $translationPrompt)
  {
    $this->translationPrompt = $translationPrompt;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPromptSpecTranslationPrompt
   */
  public function getTranslationPrompt()
  {
    return $this->translationPrompt;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPromptApiSchema::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPromptApiSchema');
