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

namespace Google\Service\CloudAlloyDBAdmin;

class PscInstanceConfig extends \Google\Collection
{
  protected $collection_key = 'pscInterfaceConfigs';
  /**
   * @var string[]
   */
  public $allowedConsumerProjects;
  /**
   * @var string
   */
  public $pscDnsName;
  protected $pscInterfaceConfigsType = PscInterfaceConfig::class;
  protected $pscInterfaceConfigsDataType = 'array';
  /**
   * @var string
   */
  public $serviceAttachmentLink;

  /**
   * @param string[]
   */
  public function setAllowedConsumerProjects($allowedConsumerProjects)
  {
    $this->allowedConsumerProjects = $allowedConsumerProjects;
  }
  /**
   * @return string[]
   */
  public function getAllowedConsumerProjects()
  {
    return $this->allowedConsumerProjects;
  }
  /**
   * @param string
   */
  public function setPscDnsName($pscDnsName)
  {
    $this->pscDnsName = $pscDnsName;
  }
  /**
   * @return string
   */
  public function getPscDnsName()
  {
    return $this->pscDnsName;
  }
  /**
   * @param PscInterfaceConfig[]
   */
  public function setPscInterfaceConfigs($pscInterfaceConfigs)
  {
    $this->pscInterfaceConfigs = $pscInterfaceConfigs;
  }
  /**
   * @return PscInterfaceConfig[]
   */
  public function getPscInterfaceConfigs()
  {
    return $this->pscInterfaceConfigs;
  }
  /**
   * @param string
   */
  public function setServiceAttachmentLink($serviceAttachmentLink)
  {
    $this->serviceAttachmentLink = $serviceAttachmentLink;
  }
  /**
   * @return string
   */
  public function getServiceAttachmentLink()
  {
    return $this->serviceAttachmentLink;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscInstanceConfig::class, 'Google_Service_CloudAlloyDBAdmin_PscInstanceConfig');
