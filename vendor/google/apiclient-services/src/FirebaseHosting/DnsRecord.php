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

namespace Google\Service\FirebaseHosting;

class DnsRecord extends \Google\Model
{
  /**
   * @var string
   */
  public $domainName;
  /**
   * @var string
   */
  public $rdata;
  /**
   * @var string
   */
  public $requiredAction;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setDomainName($domainName)
  {
    $this->domainName = $domainName;
  }
  /**
   * @return string
   */
  public function getDomainName()
  {
    return $this->domainName;
  }
  /**
   * @param string
   */
  public function setRdata($rdata)
  {
    $this->rdata = $rdata;
  }
  /**
   * @return string
   */
  public function getRdata()
  {
    return $this->rdata;
  }
  /**
   * @param string
   */
  public function setRequiredAction($requiredAction)
  {
    $this->requiredAction = $requiredAction;
  }
  /**
   * @return string
   */
  public function getRequiredAction()
  {
    return $this->requiredAction;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DnsRecord::class, 'Google_Service_FirebaseHosting_DnsRecord');
