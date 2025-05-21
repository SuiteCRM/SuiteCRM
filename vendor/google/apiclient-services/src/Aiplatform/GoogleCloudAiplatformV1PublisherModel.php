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

class GoogleCloudAiplatformV1PublisherModel extends \Google\Collection
{
  protected $collection_key = 'frameworks';
  /**
   * @var string[]
   */
  public $frameworks;
  /**
   * @var string
   */
  public $launchStage;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $openSourceCategory;
  protected $predictSchemataType = GoogleCloudAiplatformV1PredictSchemata::class;
  protected $predictSchemataDataType = '';
  /**
   * @var string
   */
  public $publisherModelTemplate;
  protected $supportedActionsType = GoogleCloudAiplatformV1PublisherModelCallToAction::class;
  protected $supportedActionsDataType = '';
  /**
   * @var string
   */
  public $versionId;
  /**
   * @var string
   */
  public $versionState;

  /**
   * @param string[]
   */
  public function setFrameworks($frameworks)
  {
    $this->frameworks = $frameworks;
  }
  /**
   * @return string[]
   */
  public function getFrameworks()
  {
    return $this->frameworks;
  }
  /**
   * @param string
   */
  public function setLaunchStage($launchStage)
  {
    $this->launchStage = $launchStage;
  }
  /**
   * @return string
   */
  public function getLaunchStage()
  {
    return $this->launchStage;
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
   * @param string
   */
  public function setOpenSourceCategory($openSourceCategory)
  {
    $this->openSourceCategory = $openSourceCategory;
  }
  /**
   * @return string
   */
  public function getOpenSourceCategory()
  {
    return $this->openSourceCategory;
  }
  /**
   * @param GoogleCloudAiplatformV1PredictSchemata
   */
  public function setPredictSchemata(GoogleCloudAiplatformV1PredictSchemata $predictSchemata)
  {
    $this->predictSchemata = $predictSchemata;
  }
  /**
   * @return GoogleCloudAiplatformV1PredictSchemata
   */
  public function getPredictSchemata()
  {
    return $this->predictSchemata;
  }
  /**
   * @param string
   */
  public function setPublisherModelTemplate($publisherModelTemplate)
  {
    $this->publisherModelTemplate = $publisherModelTemplate;
  }
  /**
   * @return string
   */
  public function getPublisherModelTemplate()
  {
    return $this->publisherModelTemplate;
  }
  /**
   * @param GoogleCloudAiplatformV1PublisherModelCallToAction
   */
  public function setSupportedActions(GoogleCloudAiplatformV1PublisherModelCallToAction $supportedActions)
  {
    $this->supportedActions = $supportedActions;
  }
  /**
   * @return GoogleCloudAiplatformV1PublisherModelCallToAction
   */
  public function getSupportedActions()
  {
    return $this->supportedActions;
  }
  /**
   * @param string
   */
  public function setVersionId($versionId)
  {
    $this->versionId = $versionId;
  }
  /**
   * @return string
   */
  public function getVersionId()
  {
    return $this->versionId;
  }
  /**
   * @param string
   */
  public function setVersionState($versionState)
  {
    $this->versionState = $versionState;
  }
  /**
   * @return string
   */
  public function getVersionState()
  {
    return $this->versionState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PublisherModel::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PublisherModel');
