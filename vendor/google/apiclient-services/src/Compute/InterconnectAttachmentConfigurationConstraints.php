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

namespace Google\Service\Compute;

class InterconnectAttachmentConfigurationConstraints extends \Google\Collection
{
  protected $collection_key = 'bgpPeerAsnRanges';
  /**
   * @var string
   */
  public $bgpMd5;
  protected $bgpPeerAsnRangesType = InterconnectAttachmentConfigurationConstraintsBgpPeerASNRange::class;
  protected $bgpPeerAsnRangesDataType = 'array';

  /**
   * @param string
   */
  public function setBgpMd5($bgpMd5)
  {
    $this->bgpMd5 = $bgpMd5;
  }
  /**
   * @return string
   */
  public function getBgpMd5()
  {
    return $this->bgpMd5;
  }
  /**
   * @param InterconnectAttachmentConfigurationConstraintsBgpPeerASNRange[]
   */
  public function setBgpPeerAsnRanges($bgpPeerAsnRanges)
  {
    $this->bgpPeerAsnRanges = $bgpPeerAsnRanges;
  }
  /**
   * @return InterconnectAttachmentConfigurationConstraintsBgpPeerASNRange[]
   */
  public function getBgpPeerAsnRanges()
  {
    return $this->bgpPeerAsnRanges;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InterconnectAttachmentConfigurationConstraints::class, 'Google_Service_Compute_InterconnectAttachmentConfigurationConstraints');
