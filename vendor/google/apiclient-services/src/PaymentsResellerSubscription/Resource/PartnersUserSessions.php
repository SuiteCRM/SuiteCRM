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

namespace Google\Service\PaymentsResellerSubscription\Resource;

use Google\Service\PaymentsResellerSubscription\GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionRequest;
use Google\Service\PaymentsResellerSubscription\GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionResponse;

/**
 * The "userSessions" collection of methods.
 * Typical usage is:
 *  <code>
 *   $paymentsresellersubscriptionService = new Google\Service\PaymentsResellerSubscription(...);
 *   $userSessions = $paymentsresellersubscriptionService->partners_userSessions;
 *  </code>
 */
class PartnersUserSessions extends \Google\Service\Resource
{
  /**
   * This API replaces user authorized OAuth consent based APIs (Create, Entitle).
   * Generates a short-lived token for a user session based on the user intent.
   * You can use the session token to redirect the user to Google to finish the
   * signup flow. You can re-generate new session token repeatedly for the same
   * request if necessary, regardless of the previous tokens being expired or not.
   * (userSessions.generate)
   *
   * @param string $parent Required. The parent, the partner that can resell.
   * Format: partners/{partner}
   * @param GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionRequest $postBody
   * @param array $optParams Optional parameters.
   * @return GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionResponse
   * @throws \Google\Service\Exception
   */
  public function generate($parent, GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionRequest $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('generate', [$params], GoogleCloudPaymentsResellerSubscriptionV1GenerateUserSessionResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PartnersUserSessions::class, 'Google_Service_PaymentsResellerSubscription_Resource_PartnersUserSessions');
