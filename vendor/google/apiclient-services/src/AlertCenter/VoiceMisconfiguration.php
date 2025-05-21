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

namespace Google\Service\AlertCenter;

class VoiceMisconfiguration extends \Google\Model
{
  /**
   * @var string
   */
  public $entityName;
  /**
   * @var string
   */
  public $entityType;
  /**
   * @var string
   */
  public $fixUri;
  protected $membersMisconfigurationType = TransferMisconfiguration::class;
  protected $membersMisconfigurationDataType = '';
  protected $transferMisconfigurationType = TransferMisconfiguration::class;
  protected $transferMisconfigurationDataType = '';
  protected $voicemailMisconfigurationType = VoicemailMisconfiguration::class;
  protected $voicemailMisconfigurationDataType = '';

  /**
   * @param string
   */
  public function setEntityName($entityName)
  {
    $this->entityName = $entityName;
  }
  /**
   * @return string
   */
  public function getEntityName()
  {
    return $this->entityName;
  }
  /**
   * @param string
   */
  public function setEntityType($entityType)
  {
    $this->entityType = $entityType;
  }
  /**
   * @return string
   */
  public function getEntityType()
  {
    return $this->entityType;
  }
  /**
   * @param string
   */
  public function setFixUri($fixUri)
  {
    $this->fixUri = $fixUri;
  }
  /**
   * @return string
   */
  public function getFixUri()
  {
    return $this->fixUri;
  }
  /**
   * @param TransferMisconfiguration
   */
  public function setMembersMisconfiguration(TransferMisconfiguration $membersMisconfiguration)
  {
    $this->membersMisconfiguration = $membersMisconfiguration;
  }
  /**
   * @return TransferMisconfiguration
   */
  public function getMembersMisconfiguration()
  {
    return $this->membersMisconfiguration;
  }
  /**
   * @param TransferMisconfiguration
   */
  public function setTransferMisconfiguration(TransferMisconfiguration $transferMisconfiguration)
  {
    $this->transferMisconfiguration = $transferMisconfiguration;
  }
  /**
   * @return TransferMisconfiguration
   */
  public function getTransferMisconfiguration()
  {
    return $this->transferMisconfiguration;
  }
  /**
   * @param VoicemailMisconfiguration
   */
  public function setVoicemailMisconfiguration(VoicemailMisconfiguration $voicemailMisconfiguration)
  {
    $this->voicemailMisconfiguration = $voicemailMisconfiguration;
  }
  /**
   * @return VoicemailMisconfiguration
   */
  public function getVoicemailMisconfiguration()
  {
    return $this->voicemailMisconfiguration;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VoiceMisconfiguration::class, 'Google_Service_AlertCenter_VoiceMisconfiguration');
