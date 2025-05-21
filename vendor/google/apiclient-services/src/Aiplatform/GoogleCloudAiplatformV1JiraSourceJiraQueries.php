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

class GoogleCloudAiplatformV1JiraSourceJiraQueries extends \Google\Collection
{
  protected $collection_key = 'projects';
  protected $apiKeyConfigType = GoogleCloudAiplatformV1ApiAuthApiKeyConfig::class;
  protected $apiKeyConfigDataType = '';
  /**
   * @var string[]
   */
  public $customQueries;
  /**
   * @var string
   */
  public $email;
  /**
   * @var string[]
   */
  public $projects;
  /**
   * @var string
   */
  public $serverUri;

  /**
   * @param GoogleCloudAiplatformV1ApiAuthApiKeyConfig
   */
  public function setApiKeyConfig(GoogleCloudAiplatformV1ApiAuthApiKeyConfig $apiKeyConfig)
  {
    $this->apiKeyConfig = $apiKeyConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ApiAuthApiKeyConfig
   */
  public function getApiKeyConfig()
  {
    return $this->apiKeyConfig;
  }
  /**
   * @param string[]
   */
  public function setCustomQueries($customQueries)
  {
    $this->customQueries = $customQueries;
  }
  /**
   * @return string[]
   */
  public function getCustomQueries()
  {
    return $this->customQueries;
  }
  /**
   * @param string
   */
  public function setEmail($email)
  {
    $this->email = $email;
  }
  /**
   * @return string
   */
  public function getEmail()
  {
    return $this->email;
  }
  /**
   * @param string[]
   */
  public function setProjects($projects)
  {
    $this->projects = $projects;
  }
  /**
   * @return string[]
   */
  public function getProjects()
  {
    return $this->projects;
  }
  /**
   * @param string
   */
  public function setServerUri($serverUri)
  {
    $this->serverUri = $serverUri;
  }
  /**
   * @return string
   */
  public function getServerUri()
  {
    return $this->serverUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1JiraSourceJiraQueries::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1JiraSourceJiraQueries');
