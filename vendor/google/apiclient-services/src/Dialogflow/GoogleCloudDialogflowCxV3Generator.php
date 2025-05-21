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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3Generator extends \Google\Collection
{
  protected $collection_key = 'placeholders';
  /**
   * @var string
   */
  public $displayName;
  protected $modelParameterType = GoogleCloudDialogflowCxV3GeneratorModelParameter::class;
  protected $modelParameterDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $placeholdersType = GoogleCloudDialogflowCxV3GeneratorPlaceholder::class;
  protected $placeholdersDataType = 'array';
  protected $promptTextType = GoogleCloudDialogflowCxV3Phrase::class;
  protected $promptTextDataType = '';

  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudDialogflowCxV3GeneratorModelParameter
   */
  public function setModelParameter(GoogleCloudDialogflowCxV3GeneratorModelParameter $modelParameter)
  {
    $this->modelParameter = $modelParameter;
  }
  /**
   * @return GoogleCloudDialogflowCxV3GeneratorModelParameter
   */
  public function getModelParameter()
  {
    return $this->modelParameter;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GoogleCloudDialogflowCxV3GeneratorPlaceholder[]
   */
  public function setPlaceholders($placeholders)
  {
    $this->placeholders = $placeholders;
  }
  /**
   * @return GoogleCloudDialogflowCxV3GeneratorPlaceholder[]
   */
  public function getPlaceholders()
  {
    return $this->placeholders;
  }
  /**
   * @param GoogleCloudDialogflowCxV3Phrase
   */
  public function setPromptText(GoogleCloudDialogflowCxV3Phrase $promptText)
  {
    $this->promptText = $promptText;
  }
  /**
   * @return GoogleCloudDialogflowCxV3Phrase
   */
  public function getPromptText()
  {
    return $this->promptText;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3Generator::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3Generator');
