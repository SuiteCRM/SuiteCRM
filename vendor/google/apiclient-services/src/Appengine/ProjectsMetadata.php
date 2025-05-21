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

namespace Google\Service\Appengine;

class ProjectsMetadata extends \Google\Collection
{
  protected $collection_key = 'gceTag';
  /**
   * @var string
   */
  public $consumerProjectId;
  /**
   * @var string
   */
  public $consumerProjectNumber;
  /**
   * @var string
   */
  public $consumerProjectState;
  protected $gceTagType = GceTag::class;
  protected $gceTagDataType = 'array';
  /**
   * @var string
   */
  public $p4ServiceAccount;
  /**
   * @var string
   */
  public $producerProjectId;
  /**
   * @var string
   */
  public $producerProjectNumber;
  /**
   * @var string
   */
  public $tenantProjectId;
  /**
   * @var string
   */
  public $tenantProjectNumber;

  /**
   * @param string
   */
  public function setConsumerProjectId($consumerProjectId)
  {
    $this->consumerProjectId = $consumerProjectId;
  }
  /**
   * @return string
   */
  public function getConsumerProjectId()
  {
    return $this->consumerProjectId;
  }
  /**
   * @param string
   */
  public function setConsumerProjectNumber($consumerProjectNumber)
  {
    $this->consumerProjectNumber = $consumerProjectNumber;
  }
  /**
   * @return string
   */
  public function getConsumerProjectNumber()
  {
    return $this->consumerProjectNumber;
  }
  /**
   * @param string
   */
  public function setConsumerProjectState($consumerProjectState)
  {
    $this->consumerProjectState = $consumerProjectState;
  }
  /**
   * @return string
   */
  public function getConsumerProjectState()
  {
    return $this->consumerProjectState;
  }
  /**
   * @param GceTag[]
   */
  public function setGceTag($gceTag)
  {
    $this->gceTag = $gceTag;
  }
  /**
   * @return GceTag[]
   */
  public function getGceTag()
  {
    return $this->gceTag;
  }
  /**
   * @param string
   */
  public function setP4ServiceAccount($p4ServiceAccount)
  {
    $this->p4ServiceAccount = $p4ServiceAccount;
  }
  /**
   * @return string
   */
  public function getP4ServiceAccount()
  {
    return $this->p4ServiceAccount;
  }
  /**
   * @param string
   */
  public function setProducerProjectId($producerProjectId)
  {
    $this->producerProjectId = $producerProjectId;
  }
  /**
   * @return string
   */
  public function getProducerProjectId()
  {
    return $this->producerProjectId;
  }
  /**
   * @param string
   */
  public function setProducerProjectNumber($producerProjectNumber)
  {
    $this->producerProjectNumber = $producerProjectNumber;
  }
  /**
   * @return string
   */
  public function getProducerProjectNumber()
  {
    return $this->producerProjectNumber;
  }
  /**
   * @param string
   */
  public function setTenantProjectId($tenantProjectId)
  {
    $this->tenantProjectId = $tenantProjectId;
  }
  /**
   * @return string
   */
  public function getTenantProjectId()
  {
    return $this->tenantProjectId;
  }
  /**
   * @param string
   */
  public function setTenantProjectNumber($tenantProjectNumber)
  {
    $this->tenantProjectNumber = $tenantProjectNumber;
  }
  /**
   * @return string
   */
  public function getTenantProjectNumber()
  {
    return $this->tenantProjectNumber;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsMetadata::class, 'Google_Service_Appengine_ProjectsMetadata');
