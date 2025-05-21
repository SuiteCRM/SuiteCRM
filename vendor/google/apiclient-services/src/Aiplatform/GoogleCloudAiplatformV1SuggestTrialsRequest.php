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

class GoogleCloudAiplatformV1SuggestTrialsRequest extends \Google\Collection
{
  protected $collection_key = 'contexts';
  /**
   * @var string
   */
  public $clientId;
  protected $contextsType = GoogleCloudAiplatformV1TrialContext::class;
  protected $contextsDataType = 'array';
  /**
   * @var int
   */
  public $suggestionCount;

  /**
   * @param string
   */
  public function setClientId($clientId)
  {
    $this->clientId = $clientId;
  }
  /**
   * @return string
   */
  public function getClientId()
  {
    return $this->clientId;
  }
  /**
   * @param GoogleCloudAiplatformV1TrialContext[]
   */
  public function setContexts($contexts)
  {
    $this->contexts = $contexts;
  }
  /**
   * @return GoogleCloudAiplatformV1TrialContext[]
   */
  public function getContexts()
  {
    return $this->contexts;
  }
  /**
   * @param int
   */
  public function setSuggestionCount($suggestionCount)
  {
    $this->suggestionCount = $suggestionCount;
  }
  /**
   * @return int
   */
  public function getSuggestionCount()
  {
    return $this->suggestionCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SuggestTrialsRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SuggestTrialsRequest');
