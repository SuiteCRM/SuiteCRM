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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1Project extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $provisionCompletionTime;
  protected $serviceTermsMapType = GoogleCloudDiscoveryengineV1ProjectServiceTerms::class;
  protected $serviceTermsMapDataType = 'map';

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
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
  public function setProvisionCompletionTime($provisionCompletionTime)
  {
    $this->provisionCompletionTime = $provisionCompletionTime;
  }
  /**
   * @return string
   */
  public function getProvisionCompletionTime()
  {
    return $this->provisionCompletionTime;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ProjectServiceTerms[]
   */
  public function setServiceTermsMap($serviceTermsMap)
  {
    $this->serviceTermsMap = $serviceTermsMap;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ProjectServiceTerms[]
   */
  public function getServiceTermsMap()
  {
    return $this->serviceTermsMap;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1Project::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1Project');
